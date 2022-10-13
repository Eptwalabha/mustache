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
    render(Template, to_map(Params), "").

render([], _, Acc) -> ?FLAT(?REV(Acc));
render([${, ${, ${ | Tail], Params, Acc) ->
    case fetch_tag_content(Tail, "}}}") of
        end_delimiter_error ->
            render(Tail, Params, [${, ${, ${ | Acc]);
        {Key, Tail2} ->
            Value = to_str(val(Key, Params)),
            render(Tail2, Params, [Value | Acc])
    end;
render([${, ${ | Tail], Params, Acc) ->
    case fetch_tag_content(Tail, "}}") of
        end_delimiter_error ->
            render(Tail, Params, [${, ${ | Acc]);
        {[$! | _], Tail2} ->
            render(Tail2, Params, Acc);
        {[Char | Key], Tail2} when Char =:= $#; Char =:= $^ ->
            {Section, Tail3} = fetch_section(Tail2, Key),
            Value = val(Key, Params),
            Rendered = case can_render(Value, is_inverted(Char)) of
                           true -> render_section(Section, Params, Value);
                           _ -> ""
                       end,
            render(Tail3, Params, [Rendered | Acc]);
        {[$> | Key], Tail2} ->
            Partial = val(Key, Params),
            Value = render(Partial, Params),
            render(Tail2, Params, [Value | Acc]);
        {Key, Tail2} ->
            Value = to_str(val(Key, Params)),
            render(Tail2, Params, [html_escape(Value, "") | Acc])
    end;
render([Letter | Tail], Params, Acc) ->
    render(Tail, Params, [Letter | Acc]).

is_inverted($^) -> true;
is_inverted(_) -> false.

can_render(Value, IsInverted) ->
    is_empty_or_false(Value) =:= IsInverted.

is_empty_or_false(Value) ->
    Value =:= [] orelse Value =:= false.

render_section(Template, Params, "") ->
    render(Template, Params);
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

fetch_tag_content(Content, Endtag) ->
    fetch_tag_content(Content, Endtag, "").

fetch_tag_content([], _, _) ->
    end_delimiter_error;
fetch_tag_content([A, B, C | Tail], [A, B, C], Acc) ->
    {string:trim(?REV(Acc)), Tail};
fetch_tag_content([A, B | Tail], [A, B], Acc) ->
    {string:trim(?REV(Acc)), Tail};
fetch_tag_content([$} | _], _, _) ->
    end_delimiter_error;
fetch_tag_content([Letter | Tail], Endtag, Acc) ->
    fetch_tag_content(Tail, Endtag, [Letter | Acc]).

val(Key, Map) when is_map(Map) ->
    maps:get(atom_key(Key), Map, "").

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

html_escape([], Acc) -> ?REV(Acc);
html_escape([$< | Tail], Acc) -> html_escape(Tail, ["&lt;" | Acc]);
html_escape([$> | Tail], Acc) -> html_escape(Tail, ["&gt;" | Acc]);
html_escape([$" | Tail], Acc) -> html_escape(Tail, ["&quot;" | Acc]);
html_escape([$& | Tail], Acc) -> html_escape(Tail, ["&amp;" | Acc]);
html_escape([Char | Tail], Acc) -> html_escape(Tail, [Char | Acc]).

fetch_section(Template, Endtag) ->
    ReOptions = [unicode, {return, list}],
    [Section | Tail] = re:split(Template, "{{/" ++ Endtag ++ "}}", ReOptions),
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

