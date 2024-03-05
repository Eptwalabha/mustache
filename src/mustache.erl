-module(mustache).

-export([render/1, render/2, render/3]).
-export([compile/1]).
-export([main/1]).

-include_lib("../include/mustache.hrl").

-record(context, { params = #{},
                   partials = #{},
                   delimiter = { "{{", "}}" } }).

main([Template | _]) ->
    io:fwrite("~ts~n", [render(Template)]).

is_empty_or_false(<<"">>) -> true;
is_empty_or_false([]) -> true;
is_empty_or_false(false) -> true;
is_empty_or_false(null) -> true;
is_empty_or_false(0) -> true;
is_empty_or_false(_) -> false.

update_params_context(#context{ params = Map } = Context, Map2)
  when is_map(Map), is_map(Map2) ->
    Context#context{ params = maps:merge(Map, Map2) };
update_params_context(#context{ params = Map } = Context, Item) ->
    Context#context{ params = maps:merge(Map, #{ '.' => Item }) }.

val(Key, #context{ params = Map}) when is_map(Map) ->
    KeyParts = key_to_parts(Key),
    fetch(KeyParts, Map, "").

% @TODO fetch partial from file if not found
get_partial(RawKey, #context{ partials = Partials })
  when is_map(Partials) ->
    Key = to_atom(RawKey),
    maps:get(Key, Partials, not_found).

key_to_parts(".") -> ['.'];
key_to_parts(Other) ->
    Key = string:trim(unicode:characters_to_list(Other)),
    [list_to_atom(P) || P <- string:lexemes(Key, ".")].

fetch([Key], Map, Default) ->
    maps:get(Key, Map, Default);
fetch([Key | Rest], Map, Default) ->
    case maps:get(Key, Map, not_found) of
        not_found -> Default;
        NewMap when is_map(NewMap) -> fetch(Rest, NewMap, Default);
        _ -> Default
    end.

atom_key(Atom) when is_atom(Atom) -> Atom;
atom_key(KeyString) -> list_to_atom(string:trim(KeyString)).

to_str(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_str(Float) when is_float(Float) ->
    io_lib:format("~p", [Float]);
to_str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_str(Binary) when is_binary(Binary) ->
    unicode:characters_to_list(Binary);
to_str(String) when is_list(String) ->
    String.

escape(Value, true) -> do_escape(Value, "");
escape(Value, _) -> Value.

do_escape([], Acc) -> ?REV(Acc);
do_escape([$< | Tail], Acc) -> do_escape(Tail, ["&lt;" | Acc]);
do_escape([$> | Tail], Acc) -> do_escape(Tail, ["&gt;" | Acc]);
do_escape([$" | Tail], Acc) -> do_escape(Tail, ["&quot;" | Acc]);
do_escape([$' | Tail], Acc) -> do_escape(Tail, ["&apos;" | Acc]);
do_escape([$& | Tail], Acc) -> do_escape(Tail, ["&amp;" | Acc]);
do_escape([Char | Tail], Acc) -> do_escape(Tail, [Char | Acc]).

to_map(Map) when is_map(Map) ->
    to_map(Map, #{});
to_map(List) when is_list(List) ->
    case is_proplist(List) of
        true -> to_map(List, #{});
        _ -> #{ '.' => List }
    end;
to_map(Other) ->
    #{ '.' => Other }.

to_map([], Map) -> Map;
to_map(Map, Acc) when is_map(Map) ->
    Fun = fun (Key, Value, MapAcc) when is_map(Value) ->
                  MapAcc#{ Key => to_map(Value, #{}) };
              (Key, [_ | _] = List, MapAcc) when is_list(List) ->
                  MapAcc#{ Key => list_to_map(List) };
              (Key, Value, MapAcc) ->
                  MapAcc#{ Key => Value }
          end,
    maps:fold(Fun, Acc, Map);
to_map([{KeyRaw, Value} | Rest], Map) ->
    Key = atom_key(KeyRaw),
    to_map(Rest, Map#{ Key => list_to_map(Value) }).

list_to_map(List) ->
    case is_proplist(List) of
        true -> to_map(List, #{});
        _ -> List
    end.

render(RawTemplate) ->
    render(RawTemplate, #{}, #{}).

render(RawTemplate, Params) ->
    render(RawTemplate, Params, #{}).

render(RawTemplate, Params, RawPartials) ->
    AST = compile(RawTemplate),
    Context = #context{ params = to_map(Params),
                        partials = prepare_partials(RawPartials) },
    do_render(AST, Context, "").

do_render([], _, Acc) -> ?FLAT(?REV(Acc));
do_render([{comment, _, _} | Tail], Context, Acc) ->
    do_render(Tail, Context, Acc);
do_render([{delimiter, _, NewDelimiter, _} | Tail], Context, Acc) ->
    do_render(Tail, Context#context{ delimiter = NewDelimiter }, Acc);
do_render([{partial, Key, _, Padding} | Tail], Context, Acc) ->
    RenderedPartial = render_partial(Key, Context, Padding),
    do_render(Tail, Context, [RenderedPartial | Acc]);
do_render([{dynamic, Key, _, Padding} | Tail], Context, Acc) ->
    case val(Key, Context) of
        "" -> do_render(Tail, Context, Acc);
        RawPartialKey ->
            PartialKey = to_atom(RawPartialKey),
            RenderedPartial = render_partial(PartialKey, Context, Padding),
            do_render(Tail, Context, [RenderedPartial | Acc])
    end;
do_render([{new_line, NewLine} | Tail], Context, Acc) ->
    do_render(Tail, Context, [NewLine | Acc]);
do_render([{SectionType, Key, SectionAST, _} | Tail], Context, Acc)
  when SectionType =:= section;
       SectionType =:= inverted ->
    Value = val(Key, Context),
    IsInverted = (SectionType =:= inverted),
    Rendered = case is_empty_or_false(Value) of
                   IsInverted -> render_section(SectionAST, Context, Value);
                   _ -> []
               end,
    do_render(Tail, Context, Rendered ++ Acc);
do_render([{SubstitutionType, Key, _} | Tail], Context, Acc)
  when SubstitutionType =:= variable;
       SubstitutionType =:= no_escape ->
    Value = val(Key, Context),
    case is_empty_or_false(Value) of
        true -> do_render(Tail, Context, Acc);
        _ ->
            Escape = SubstitutionType =:= variable,
            Value2 = substitute(Value, Context, Escape),
            do_render(Tail, Context, [Value2 | Acc])
    end;
do_render([{ignore, _} | Tail], Context, Acc) ->
    do_render(Tail, Context, Acc);
do_render([List | Tail], Context, Acc) when is_list(List) ->
    do_render(Tail, Context, [List | Acc]).

substitute(Lambda, Context, Escape) when is_function(Lambda, 0) ->
    AST = compile(to_str(Lambda())),
    Value = do_render(AST, Context, []),
    escape(Value, Escape);
substitute(RawValue, _, Escape) ->
    Value = to_str(RawValue),
    escape(Value, Escape).

render_partial(PartialKey, Context, Padding) ->
    case get_partial(PartialKey, Context) of
        not_found -> "";
        Partial ->
            AST = compile(Partial),
            do_render(add_padding(AST, Padding), Context, [])
    end.

add_padding(AST, "") -> AST;
add_padding(AST, Pad) ->
    add_padding([Pad | AST], Pad, []).

add_padding([], _, Acc) -> ?REV(Acc);
add_padding([{new_line, _} = NewLine], Pad, Acc) ->
    add_padding([], Pad, [NewLine | Acc]);
add_padding([{new_line, _} = NewLine | Tail], Pad, Acc) ->
    add_padding(Tail, Pad, [Pad, NewLine | Acc]);
add_padding([Element | Tail], Pad, Acc) ->
    add_padding(Tail, Pad, [Element | Acc]).


render_section(AST, Context, Lambda)
  when is_function(Lambda, 1);
       is_function(Lambda, 2) ->
    Template = mustache_ast:ast_to_template(AST),
    {StartTag, EndTag} = Context#context.delimiter,
    Options = #{ start_tag => StartTag, end_tag => EndTag },
    NewTemplate = get_lambda_template(Context, Template, Lambda, Options),
    ?REV(do_render(compile(NewTemplate, Options), Context, []));
render_section(AST, Context, "") ->
    NewContext = update_params_context(Context, ""),
    ?REV(do_render(AST, NewContext, []));
render_section(AST, Context, Value)
  when is_list(Value) ->
    Fun = fun (Item) ->
                  NewContext = update_params_context(Context, Item),
                  do_render(AST, NewContext, [])
          end,
    ?REV(lists:map(Fun, Value));
render_section(AST, Context, Value) ->
    NewContext = update_params_context(Context, Value),
    ?REV(do_render(AST, NewContext, [])).

get_lambda_template(_, Template, Lambda, _)
  when is_function(Lambda, 1) ->
    Lambda(Template);
get_lambda_template(Context, Template, Lambda, Options)
  when is_function(Lambda, 2) ->
    NewContext = update_params_context(Context, Lambda),
    Renderer = fun (NewTemplate) ->
                       NewAST = compile(NewTemplate, Options),
                       do_render(NewAST, NewContext, [])
               end,
    Lambda(Template, Renderer).


compile(Content) ->
    Options = #{ start_tag => "{{", end_tag => "}}" },
    compile(Content, Options).

compile(RawTemplate, Options) ->
    Template = unicode:characters_to_list(RawTemplate),
    Tokens = mustache_scan:tokens(Template, Options),
    mustache_ast:build(Tokens).

prepare_partials(RawPartials) ->
    Fun = fun (RawKey, RawPartial, MapAcc) ->
                  Key = to_atom(RawKey),
                  MapAcc#{Key => unicode:characters_to_list(RawPartial)}
          end,
    maps:fold(Fun, #{}, RawPartials).

is_proplist(List) when is_list(List) ->
    lists:all(fun ({_, _}) -> true; (_) -> false end, List);
is_proplist(_) ->
    false.

to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(List) when is_list(List) -> list_to_atom(List);
to_atom(Binary) when is_binary(Binary) -> to_atom(binary_to_list(Binary)).
