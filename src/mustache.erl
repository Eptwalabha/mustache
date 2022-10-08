-module(mustache).

-export([render/1, render/2]).
-export([main/1]).

-define(rev(L), lists:reverse(L)).
-define(val(K, P), proplists:get_value(K, P)).
-define(val(K, P, D), proplists:get_value(K, P, D)).

main([Template | _]) ->
    io:format("~ts~n", [render(Template)]).

render(Template) ->
    render(Template, []).

render(Template, Params) ->
    render(Template, Params, "").

render([], _, Acc) -> lists:flatten(?rev(Acc));
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
            Is_inverted = Char =:= $^,
            Value = val(Key, Params),
            Rendered = case can_render(Value, Is_inverted) of
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

-spec can_render(any(), Is_inverted::boolean()) -> boolean().
can_render([], true) -> true;
can_render(false, true) -> true;
can_render(_, true) -> false;
can_render([], false) -> false;
can_render(false, false) -> false;
can_render(_, _) -> true.

render_section(Template, Params, Lambda) when is_function(Lambda, 2) ->
    Render = fun (New_template) -> render(New_template, Params) end,
    render(Lambda(Template, Render), Params);
render_section(Template, Params, "") ->
    render(Template, Params);
render_section(Template, Params, Value) when is_list(Value) ->
    case is_string(Value) of
        true -> render(Template, Params);
        _ ->
            Fun = fun ([{_, _} | _] = Item) ->
                          render(Template, Item ++ Params);
                      (Item) ->
                          render(Template, [{'.', Item} | Params])
                  end,
            lists:map(Fun, Value)
    end;
render_section(Template, Params, _) ->
    render(Template, Params).

fetch_tag_content(Content, Endtag) ->
    fetch_tag_content(Content, Endtag, "").

fetch_tag_content([], _, _) ->
    end_delimiter_error;
fetch_tag_content([A, B, C | Tail], [A, B, C], Acc) ->
    {string:trim(?rev(Acc)), Tail};
fetch_tag_content([A, B | Tail], [A, B], Acc) ->
    {string:trim(?rev(Acc)), Tail};
fetch_tag_content([$} | _], _, _) ->
    end_delimiter_error;
fetch_tag_content([Letter | Tail], Endtag, Acc) ->
    fetch_tag_content(Tail, Endtag, [Letter | Acc]).

val(Key_atom, Params) when is_atom(Key_atom) ->
    ?val(Key_atom, Params, "");
val(Key_str, Params) ->
    val(list_to_atom(string:trim(Key_str)), Params).

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

html_escape([], Acc) -> ?rev(Acc);
html_escape([$< | Tail], Acc) -> html_escape(Tail, ["&lt;" | Acc]);
html_escape([$> | Tail], Acc) -> html_escape(Tail, ["&gt;" | Acc]);
html_escape([Char | Tail], Acc) -> html_escape(Tail, [Char | Acc]).

fetch_section(Template, Endtag) ->
    Re_options = [unicode, {return, list}],
    [Section | Tail] = re:split(Template, "{{/" ++ Endtag ++ "}}", Re_options),
    {Section, Tail}.

is_string(String) -> lists:all(fun(X) -> is_integer(X) end, String).

