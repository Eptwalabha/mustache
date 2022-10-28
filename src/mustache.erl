-module(mustache).

-export([render/1, render/2]).
-export([compile/1]).
-export([main/1]).

-define(REV(L), lists:reverse(L)).
-define(FLAT(L), lists:flatten(L)).

main([Template | _]) ->
    io:fwrite("~ts~n", [render(Template)]).

is_empty_or_false(Value) ->
    lists:member(Value, [<<"">>, [], false, null]).

update_params_context(Map, Map2)
  when is_map(Map), is_map(Map2) ->
    maps:merge(Map, Map2);
update_params_context(Map, Item) ->
    maps:merge(Map, #{ '.' => Item }).

parse_tag_name([$! | Comment]) -> {comment, string:trim(Comment)};
parse_tag_name([$# | Tag]) -> {section, string:trim(Tag)};
parse_tag_name([$^ | Tag]) -> {inverted, string:trim(Tag)};
parse_tag_name([$& | Tag]) -> {no_escape, string:trim(Tag)};
parse_tag_name([$> | Tag]) -> {partial, string:trim(Tag)};
parse_tag_name([$/ | Tag]) -> {end_section, string:trim(Tag)};
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
                  case is_proplist(List) of
                      true -> MapAcc#{ Key => to_map(List, #{}) };
                      _ -> MapAcc#{ Key => List }
                  end;
              (Key, Value, MapAcc) ->
                  MapAcc#{ Key => Value }
          end,
    maps:fold(Fun, Acc, Map);
to_map([{KeyRaw, Value} | Rest], Map) ->
    Key = atom_key(KeyRaw),
    Map2 = case is_proplist(Value) of
               true -> Map#{ Key => to_map(Value, #{})};
               _ -> Map#{ Key => Value }
           end,
    to_map(Rest, Map2).

render(RawTemplate) ->
    render(RawTemplate, #{}).

render(RawTemplate, Params) ->
    Template = unicode:characters_to_list(RawTemplate),
    Ast = compile(Template),
    render(Ast, to_map(Params), "").

render([], _, Acc) -> ?FLAT(?REV(Acc));
render([new_line | Tail], Params, Acc) ->
    render(Tail, Params, [$\n | Acc]);
render([{SectionType, Key, SectionAST, _} | Tail], Params, Acc)
  when SectionType =:= section;
       SectionType =:= inverted ->
    Value = val(Key, Params),
    IsInverted = (SectionType =:= inverted),
    Rendered = case is_empty_or_false(Value) of
                   IsInverted -> render_section(SectionAST, Params, Value);
                   _ -> []
               end,
    render(Tail, Params, Rendered ++ Acc);
render([{SubstitutionType, Key, _} | Tail], Params, Acc)
  when SubstitutionType =:= variable;
       SubstitutionType =:= no_escape ->
    Value = val(Key, Params),
    case is_empty_or_false(Value) of
        true -> render(Tail, Params, Acc);
        _ ->
            Escape = SubstitutionType =:= variable,
            Value2 = substitute(Value, Params, Escape),
            render(Tail, Params, [Value2 | Acc])
    end;
render([List | Tail], Params, Acc) when is_list(List) ->
    render(Tail, Params, [List | Acc]);
render([_ | Tail], Params, Acc) ->
    render(Tail, Params, Acc).

substitute(Lambda, Params, Escape) when is_function(Lambda, 0) ->
    Value = render(to_str(Lambda()), Params),
    case Escape of
        true -> html_escape(Value, "");
        _ -> Value
    end;
substitute(RawValue, _, Escape) ->
    Value = to_str(RawValue),
    case Escape of
        true -> html_escape(Value, "");
        _ -> Value
    end.

render_section(AST, Params, Lambda)
  when is_function(Lambda, 1);
       is_function(Lambda, 2) ->
    Template = ast_to_template(AST),
    NewTemplate = case is_function(Lambda, 1) of
                      true -> Lambda(Template);
                      _ ->
                          NewParams = update_params_context(Params, Lambda),
                          Renderer = fun (NewTemplate) ->
                                             NewAST = compile(NewTemplate),
                                             render(NewAST, NewParams, [])
                                     end,
                          Lambda(Template, Renderer)
                  end,
    ?REV(render(compile(NewTemplate), Params, []));
render_section(AST, Params, "") ->
    NewParams = update_params_context(Params, ""),
    ?REV(render(AST, NewParams, []));
render_section(AST, Params, Value)
  when is_list(Value) ->
    Fun = fun (Item) ->
                  NewParams = update_params_context(Params, Item),
                  render(AST, NewParams, [])
          end,
    ?REV(lists:map(Fun, Value));
render_section(AST, Params, Value) ->
    NewParams = update_params_context(Params, Value),
    ?REV(render(AST, NewParams, [])).

compile(Content) ->
    Options = #{ start_tag => "{{", end_tag => "}}" },
    Tokens = tokenize(Content, Options, null),
    Cleaned_tokens = clean(Tokens),
    {Ast, _, _} = ast(Cleaned_tokens, null, []),
    Ast.

tokenize([], _, {[], Acc}) -> ?REV(Acc);
tokenize([], _, {LineAcc, Acc}) ->
    ?REV([?REV(commit_line(LineAcc)) | Acc]);
tokenize([$\n | Tail], Options, Acc) ->
    Acc2 = push_acc(new_line, Acc),
    tokenize(Tail, Options, Acc2);
tokenize([${, ${, ${ | Tail] = Template, #{ start_tag := "{{" } = Options, Acc) ->
    case tag_tokenize(Template, #{ start_tag => "{{{", end_tag => "}}}" }) of
        {TagType, TagName, TagContent, Tail2} ->
            Acc2 = push_acc({TagType, TagName, TagContent}, Acc),
            tokenize(Tail2, Options, Acc2);
        error ->
            Acc2 = push_acc({string, "{{{"}, Acc),
            tokenize(Tail, Options, Acc2)
    end;
tokenize([Letter | Tail] = Template, Options, Acc) ->
    case is_start_tag(Template, Options) of
        true ->
            case tag_tokenize(Template, Options) of
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
    case parse_tag(Template, {Start, End}) of
        missing_end_tag -> error;
        {[$= | _] = TagContent, Tail} ->
            case string:lexemes(string:trim(TagContent, both, "="), " \n\t") of
                [NewStartTag, NewEndTag] ->
                    {delimiter, {NewStartTag, NewEndTag},
                     Start ++ TagContent ++ End, Tail};
                _ -> error
            end;
        {TagContent, Tail} ->
            case parse_tag_name(TagContent) of
                {TagType, TagName} ->
                    {TagType, TagName, Start ++ TagContent ++ End, Tail};
                TagName ->
                    TagType = case Start of
                                  "{{{" -> no_escape;
                                  _ -> variable
                              end,
                    {TagType, TagName, Start ++ TagContent ++ End, Tail}
            end;
        _ -> missing_end_tag
    end.

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

is_start_tag([A | _], #{ start_tag := [A] }) -> true;
is_start_tag([A, B | _], #{ start_tag := [A, B] }) -> true;
is_start_tag([A, B, C | _], #{ start_tag := [A, B, C] }) -> true;
is_start_tag(_, _) -> false.

parse_tag([A | Tail], {[A], EndTag}) ->
    parse_tag(Tail, EndTag, "");
parse_tag([A, B | Tail], {[A, B], EndTag}) ->
    parse_tag(Tail, EndTag, "");
parse_tag([A, B, C | Tail], {[A, B, C], EndTag}) ->
    parse_tag(Tail, EndTag, "");
parse_tag(_, _) -> missmatch_start_tag.

parse_tag([A | Tail], [A], Acc) -> {?REV(Acc), Tail};
parse_tag([A, B | Tail], [A, B], Acc) -> {?REV(Acc), Tail};
parse_tag([A, B, C | Tail], [A, B, C], Acc) -> {?REV(Acc), Tail};
parse_tag([_], [_], _) -> missing_end_tag;
parse_tag([_, _], [_, _], _) -> missing_end_tag;
parse_tag([_, _, _], [_, _, _], _) -> missing_end_tag;
parse_tag([L | Tail], EndTag, Acc) ->
    parse_tag(Tail, EndTag, [L | Acc]).

clean(Lines) ->
    lists:flatmap(fun clean_tokens/1, Lines).

clean_tokens([new_line]) -> [new_line];
clean_tokens(new_line) -> [new_line];
clean_tokens(Line) ->
    case trim(Line) of
        [] -> [];
        [{section, _, _} = Section] -> [Section];
        [{inverted, _, _} = Section] -> [Section];
        [{end_section, _, _} = Section] -> [Section];
        _ -> Line
    end.

trim(Tokens) ->
    Tokens2 = trim_tokens(Tokens),
    ?REV(trim_tokens(?REV(Tokens2))).

trim_tokens([]) -> [];
trim_tokens([{blank, _} | Tokens]) -> trim_tokens(Tokens);
trim_tokens([{comment, _, _} | Tokens]) -> trim_tokens(Tokens);
trim_tokens([{delimiter, _, _, _} | Tokens]) -> trim_tokens(Tokens);
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
process_item({delimiter, _, _, _}, Acc) -> Acc;
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

