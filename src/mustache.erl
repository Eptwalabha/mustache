-module(mustache).

-export([render/1, render/2, render/3]).
-export([compile/1]).
-export([main/1]).

-define(REV(L), lists:reverse(L)).
-define(FLAT(L), lists:flatten(L)).

-record(context, { params = #{},
                   partials = #{},
                   delimiter = { "{{", "}}" } }).

main([Template | _]) ->
    io:fwrite("~ts~n", [render(Template)]).

is_empty_or_false(Value) ->
    lists:member(Value, [<<"">>, [], false, null]).

update_params_context(#context{ params = Map } = Context, Map2)
  when is_map(Map), is_map(Map2) ->
    Context#context{ params = maps:merge(Map, Map2) };
update_params_context(#context{ params = Map } = Context, Item) ->
    Context#context{ params = maps:merge(Map, #{ '.' => Item }) }.

parse_tag_name([$! | Comment]) -> {comment, string:trim(Comment)};
parse_tag_name([$# | Tag]) -> {section, string:trim(Tag)};
parse_tag_name([$^ | Tag]) -> {inverted, string:trim(Tag)};
parse_tag_name([$& | Tag]) -> {no_escape, string:trim(Tag)};
parse_tag_name([$> | Tag]) ->
    case string:trim(Tag) of
        [$* | TagName] -> {dynamic, string:trim(TagName)};
        TagName -> {partial, TagName}
    end;
parse_tag_name([$/ | Tag]) -> {end_section, string:trim(Tag)};
parse_tag_name(Tag) -> string:trim(Tag).

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
do_render([{delimiter, _, NewDelimiter, _} | Tail], Context, Acc) ->
    do_render(Tail, Context#context{ delimiter = NewDelimiter }, Acc);
do_render([{partial, Key, _} | Tail], Context, Acc) ->
    RenderedPartial = render_partial(Key, Context),
    do_render(Tail, Context, [RenderedPartial | Acc]);
do_render([{dynamic, Key, _} | Tail], Context, Acc) ->
    case val(Key, Context) of
        "" -> do_render(Tail, Context, Acc);
        RawPartialKey ->
            PartialKey = to_atom(RawPartialKey),
            RenderedPartial = render_partial(PartialKey, Context),
            do_render(Tail, Context, [RenderedPartial | Acc])
    end;
do_render([new_line | Tail], Context, Acc) ->
    do_render(Tail, Context, [$\n | Acc]);
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
do_render([List | Tail], Context, Acc) when is_list(List) ->
    do_render(Tail, Context, [List | Acc]).

substitute(Lambda, Context, Escape) when is_function(Lambda, 0) ->
    Ast = compile(to_str(Lambda())),
    Value = do_render(Ast, Context, []),
    escape(Value, Escape);
substitute(RawValue, _, Escape) ->
    Value = to_str(RawValue),
    escape(Value, Escape).

render_partial(PartialKey, Context) ->
    case get_partial(PartialKey, Context) of
        not_found -> "";
        Partial ->
            AST = compile(Partial),
            do_render(AST, Context, [])
    end.

render_section(AST, Context, Lambda)
  when is_function(Lambda, 1);
       is_function(Lambda, 2) ->
    Template = ast_to_template(AST),
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
    Tokens = tokenize(Template, Options, null),
    CleanedTokens = clean(Tokens),
    {Ast, _, _} = ast(CleanedTokens, null, []),
    Ast.

prepare_partials(RawPartials) ->
    Fun = fun (RawKey, RawPartial, MapAcc) ->
                  Key = to_atom(RawKey),
                  MapAcc#{Key => unicode:characters_to_list(RawPartial)}
          end,
    maps:fold(Fun, #{}, RawPartials).

tokenize([], _, null) -> "";
tokenize([], _, {[], Acc}) -> ?REV(Acc);
tokenize([], _, {LineAcc, Acc}) ->
    ?REV([?REV(commit_line(LineAcc)) | Acc]);
tokenize([$\n | Tail], Options, Acc) ->
    Acc2 = push_acc(new_line, Acc),
    tokenize(Tail, Options, Acc2);
tokenize([${, ${, ${ | Tail], #{ start_tag := "{{" } = Options, Acc) ->
    case tag_tokenize(Tail, #{ start_tag => "{{{", end_tag => "}}}" }) of
        {TagType, TagName, TagContent, Tail2} ->
            Acc2 = push_acc({TagType, TagName, TagContent}, Acc),
            tokenize(Tail2, Options, Acc2);
        error ->
            Acc2 = push_acc({string, "{{{"}, Acc),
            tokenize(Tail, Options, Acc2)
    end;
tokenize([Letter | Tail] = Template,
         #{ start_tag := StartTag } = Options, Acc) ->
    case start_with(Template, StartTag) of
        {true, TagTail} ->
            case tag_tokenize(TagTail, Options) of
                error ->
                    Acc2 = push_acc({string, [Letter]}, Acc),
                    tokenize(Tail, Options, Acc2);
                {delimiter, {NewStartTag, NewEndTag}, TagContent, Tail2} ->
                    #{ start_tag := StartTag, end_tag := EndTag } = Options,
                    TagInfos = {delimiter, {StartTag, EndTag},
                                {NewStartTag, NewEndTag}, TagContent},
                    Acc2 = push_acc(TagInfos, Acc),
                    Options2 = Options#{ start_tag => NewStartTag,
                                         end_tag => NewEndTag },
                    tokenize(Tail2, Options2, Acc2);
                {TagType, TagName, TagContent, Tail2} ->
                    Acc2 = push_acc({TagType, TagName, TagContent}, Acc),
                    tokenize(Tail2, Options, Acc2)
            end;
        _ ->
            Acc2 = push_acc({string, [Letter]}, Acc),
            tokenize(Tail, Options, Acc2)
    end.

tag_tokenize(Template, #{ start_tag := Start, end_tag := End }) ->
    case fetch_tag_content(Template, End, "") of
        missing_end_tag -> error;
        {[$= | _] = TagContent, Tail} ->
            case string:lexemes(string:trim(TagContent, both, "="), " \n\t") of
                [NewStartTag, NewEndTag] ->
                    {delimiter, {NewStartTag, NewEndTag},
                     Start ++ TagContent ++ End, Tail};
                _ -> error
            end;
        {TagContent, Tail} ->
            FullTagContent = Start ++ TagContent ++ End,
            case parse_tag_name(TagContent) of
                {TagType, TagName} ->
                    {TagType, TagName, FullTagContent, Tail};
                TagName ->
                    {tag_type(Start), TagName, FullTagContent, Tail}
            end
    end.

tag_type("{{{") -> no_escape;
tag_type(_) -> variable.

push_acc({string, Letters}, null) -> {[{string, ?REV(Letters)}], []};
push_acc(new_line, null) -> {[], [new_line]};
push_acc(Other, null) -> {[Other], []};
push_acc(new_line, {LineAcc, Acc}) ->
    Line = [new_line | commit_line(LineAcc)],
    {[], [?REV(Line) | Acc]};
push_acc({string, Letters}, {[{string, String} | LineAcc], Acc}) ->
    {[{string, ?REV(Letters) ++ String} | LineAcc], Acc};
push_acc(Other, {LineAcc, Acc}) ->
    {[Other | commit_line(LineAcc)], Acc}.

commit_line([{string, RevString} | Acc]) ->
    String = ?REV(RevString),
    case string:trim(String) of
        "" -> [{blank, String} | Acc];
        _ -> [{string, String} | Acc]
    end;
commit_line(Acc) -> Acc.

start_with(Tail, []) -> {true, Tail};
start_with([Same | Tail], [Same | Tail2]) -> start_with(Tail, Tail2);
start_with(_, _) -> false.

fetch_tag_content([], _, _) ->
    missing_end_tag;
fetch_tag_content([Letter | Tail] = TagContent, EndTag, Acc) ->
    case start_with(TagContent, EndTag) of
        {true, Tail2} -> {?REV(Acc), Tail2};
        _ -> fetch_tag_content(Tail, EndTag, [Letter | Acc])
    end.

clean(Lines) ->
    lists:flatmap(fun clean_tokens/1, Lines).

clean_tokens([new_line]) -> [new_line];
clean_tokens(new_line) -> [new_line];
clean_tokens(Line) ->
    case trim(Line) of
        [] -> [];
        [{delimiter, _, _, _} = Delimiter] -> [Delimiter];
        [{section, _, _} = Section] -> [Section];
        [{inverted, _, _} = Section] -> [Section];
        [{end_section, _, _} = Section] -> [Section];
        [{partial, _, _} = Partial] -> [Partial];
        [{dynamic, _, _} = Partial] -> [Partial];
        _ -> Line
    end.

trim(Tokens) ->
    Tokens2 = trim_tokens(Tokens),
    ?REV(trim_tokens(?REV(Tokens2))).

trim_tokens([]) -> [];
trim_tokens([{blank, _} | Tokens]) -> trim_tokens(Tokens);
trim_tokens([{comment, _, _} | Tokens]) -> trim_tokens(Tokens);
trim_tokens([new_line | Tokens]) -> trim_tokens(Tokens);
trim_tokens(Tokens) -> Tokens.

ast([], _, Acc) -> {?REV(Acc), "", []};
ast([{end_section, SectionName, I} | Tail], SectionName, Acc) ->
    {?REV(Acc), I, Tail};
ast([{Section, SubSectionName, StartTag} | Tail], SectionName, Acc)
  when Section =:= section;
       Section =:= inverted ->
    {SubSectionAst, EndTag, Tail2} = ast(Tail, SubSectionName, []),
    Item = {Section, SubSectionName, SubSectionAst, {StartTag, EndTag}},
    ast(Tail2, SectionName, [Item | Acc]);
ast([Item | Tail], SectionName, Acc) ->
    ast(Tail, SectionName, process_item(Item, Acc)).

process_item({string, String}, Acc) -> [String | Acc];
process_item({blank, String}, Acc) -> [String | Acc];
process_item({comment, _, _}, Acc) -> Acc;
process_item(Other, Acc) -> [Other | Acc].

ast_to_template(List) ->
    ?FLAT(ast_to_template(List, "")).

ast_to_template([], Acc) -> ?REV(Acc);
ast_to_template([{Section, _, Ast, {Open, Close}} | Tail], Acc)
  when Section =:= section;
       Section =:= inverted ->
    Template = [Open, ast_to_template(Ast, []), Close],
    ast_to_template(Tail, [?REV(Template) | Acc]);
ast_to_template([{_, _, Txt} | Tail], Acc) ->
    ast_to_template(Tail, [Txt | Acc]);
ast_to_template([new_line | Tail], Acc) ->
    ast_to_template(Tail, [$\n | Acc]);
ast_to_template([List | Tail], Acc) when is_list(List) ->
    case is_string(List) of
        true -> ast_to_template(Tail, [List | Acc]);
        _ ->
            Txt = lists:map(fun ast_to_template/1, List),
            ast_to_template(Tail, [Txt | Acc])
    end.

is_string(List) when is_list(List) ->
    lists:all(fun (X) -> is_integer(X) end, ?FLAT(List)).

is_proplist(List) when is_list(List) ->
    lists:all(fun ({_, _}) -> true; (_) -> false end, List);
is_proplist(_) ->
    false.

to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(List) when is_list(List) -> list_to_atom(List);
to_atom(Binary) when is_binary(Binary) -> to_atom(binary_to_list(Binary)).
