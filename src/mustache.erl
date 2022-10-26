-module(mustache).

-export([render/1, render/2]).
-export([main/1]).

-define(REV(L), lists:reverse(L)).
-define(FLAT(L), lists:flatten(L)).

main([Template | _]) ->
    io:fwrite("~ts~n", [render(Template)]).

render(Template) ->
    render(Template, []).

render(Template, Params) ->
    render(unicode:characters_to_list(Template), to_map(Params), "").

render([], _, Acc) -> ?FLAT(?REV(Acc));
render([${, ${, ${ | Tail], Params, Acc) ->
    case parse_tag(Tail, "{{{") of
        missing_end_tag ->
            render(Tail, Params, [${, ${, ${ | Acc]);
        {Key, _, Tail2} ->
            Value = to_str(val(Key, Params)),
            render(Tail2, Params, [Value | Acc])
    end;
render([${, ${ | Tail], Params, Acc) ->
    case parse_tag(Tail, "{{") of
        missing_end_tag ->
            render(Tail, Params, [${, ${ | Acc]);
        {comment, _, Tail2} ->
            render(Tail2, Params, Acc);
        {{Section_type, Key}, _, Tail2}
          when Section_type =:= section;
               Section_type =:= inverted ->
            {Section, Tail3} = fetch_section(Tail2, Key),
            Value = val(Key, Params),
            Rendered = case can_render(Value, Section_type =:= inverted) of
                           true -> render_section(Section, Params, Value);
                           _ -> ""
                       end,
            render(Tail3, Params, [Rendered | Acc]);
        {{partial, Key}, _, Tail2} ->
            Partial = val(Key, Params),
            Value = render(Partial, Params),
            render(Tail2, Params, [Value | Acc]);
        {{no_escape, Key}, _, Tail2} ->
            Value = to_str(val(Key, Params)),
            render(Tail2, Params, [Value, Acc]);
        {Key, _, Tail2} ->
            Value = to_str(val(Key, Params)),
            render(Tail2, Params, [html_escape(Value, "") | Acc])
    end;
render([Letter | Tail], Params, Acc) ->
    render(Tail, Params, [Letter | Acc]).

can_render(Value, IsInverted) ->
    is_empty_or_false(Value) =:= IsInverted.

is_empty_or_false(Value) ->
    Value =:= [] orelse Value =:= false orelse Value =:= null.

render_section(Template, Params, "") ->
    render(Template, Params);
render_section(Template, Params, Lambda)
  when is_function(Lambda, 1) ->
    render(Lambda(Template), Params);
render_section(Template, Params, Lambda)
  when is_function(Lambda, 2) ->
    Render = fun (NewTemplate) -> render(NewTemplate, Params) end,
    render(Lambda(Template, Render), Params);
render_section(Template, Params, Value)
  when is_list(Value) ->
    Fun = fun (Item) ->
                  NewParams = update_params_context(Params, Item),
                  render(Template, NewParams)
          end,
    lists:map(Fun, Value);
render_section(Template, Params, Value) ->
    NewParams = update_params_context(Params, Value),
    render(Template, NewParams).

update_params_context(Map, Map2)
  when is_map(Map), is_map(Map2) ->
    maps:merge(Map, Map2);
update_params_context(Map, Item) ->
    maps:merge(Map, #{ '.' => Item }).

parse_tag(Content, StartTag) ->
    EndTag = case StartTag of
                 "{{{" -> "}}}";
                 "{{" -> "}}"
             end,
    case parse_tag(Content, EndTag, "") of
        missing_end_tag -> missing_end_tag;
        {RawTag, Tail} ->
            FullTag = StartTag ++ RawTag ++ EndTag,
            {parse_tag_name(RawTag), FullTag, Tail}
    end.

parse_tag([], _, _) ->
    missing_end_tag;
parse_tag([A, B, C | Tail], [A, B, C], Acc) ->
    {?REV(Acc),  Tail};
parse_tag([A, B | Tail], [A, B], Acc) ->
    {?REV(Acc), Tail};
parse_tag([$} | _], _, _) ->
    missing_end_tag;
parse_tag([Letter | Tail], EndTag, Acc) ->
    parse_tag(Tail, EndTag, [Letter | Acc]).

parse_tag_name([$! | _]) -> comment;
parse_tag_name([$# | Tag]) -> {section, string:trim(Tag)};
parse_tag_name([$^ | Tag]) -> {inverted, string:trim(Tag)};
parse_tag_name([$& | Tag]) -> {no_escape, string:trim(Tag)};
parse_tag_name([$> | Tag]) -> {partial, string:trim(Tag)};
parse_tag_name(Tag) -> string:trim(Tag).

val(Key, Map) when is_map(Map) ->
    KeyParts = key_to_parts(Key),
    fetch(KeyParts, Map, "").

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
to_str(null) -> "";
to_str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_str(Binary) when is_binary(Binary) ->
    unicode:characters_to_list(Binary);
to_str(String) when is_list(String) ->
    String.

html_escape([], Acc) -> ?REV(Acc);
html_escape([$< | Tail], Acc) -> html_escape(Tail, ["&lt;" | Acc]);
html_escape([$> | Tail], Acc) -> html_escape(Tail, ["&gt;" | Acc]);
html_escape([$" | Tail], Acc) -> html_escape(Tail, ["&quot;" | Acc]);
html_escape([$' | Tail], Acc) -> html_escape(Tail, ["&apos;" | Acc]);
html_escape([$& | Tail], Acc) -> html_escape(Tail, ["&amp;" | Acc]);
html_escape([Char | Tail], Acc) -> html_escape(Tail, [Char | Acc]).

fetch_section(Template, EndTag) ->
    ReOptions = [unicode, {return, list}],
    [Section | Tail] = re:split(Template, "{{/" ++ EndTag ++ "}}", ReOptions),
    {Section, ?FLAT(Tail)}.

to_map(Map) when is_map(Map) -> Map;
to_map(Proplist) ->
    to_map(Proplist, #{}).

to_map([], Map) -> Map;
to_map([{KeyRaw, Value} | Rest], Map) ->
    Key = atom_key(KeyRaw),
    Map2 = case is_proplist(Value) of
               true -> Map#{ Key => to_map(Value, #{})};
               _ -> Map#{ Key => Value }
           end,
    to_map(Rest, Map2).

is_proplist(List) when is_list(List) ->
    lists:all(fun ({_, _}) -> true; (_) -> false end, List);
is_proplist(_) -> false.

