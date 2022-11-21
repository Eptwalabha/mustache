-module(mustache_ast).

-export([build/1]).
-export([ast_to_template/1]).

-include_lib("../include/mustache.hrl").

build(Tokens) ->
    {AST, _, _} = ast(Tokens, null, []),
    AST.

ast([], null, Acc) -> {?REV(Acc), "", []};
ast([], SectionName, _) ->
    error({missing_end_section, SectionName});
ast([{ignore, _} | Tail], SectionName, Acc) ->
    ast(Tail, SectionName, Acc);
ast([{end_section, SectionName, I} | Tail], SectionName, Acc) ->
    {?REV(Acc), I, Tail};
ast([{Section, SubSectionName, TagContent} | Tail], SectionName, Acc)
  when Section =:= section;
       Section =:= inverted ->
    {SubSectionAst, EndTag, Tail2} = ast(Tail, SubSectionName, []),
    Item = {Section, SubSectionName, SubSectionAst, {TagContent, EndTag}},
    ast(Tail2, SectionName, [Item | Acc]);
ast([Item | Tail], SectionName, Acc) ->
    ast(Tail, SectionName, process_item(Item, Acc)).

process_item({string, String}, Acc) -> [String | Acc];
process_item({blank, String}, Acc) -> [String | Acc];
process_item({comment, _, _} = Comment, Acc) -> [Comment | Acc];
process_item(Other, Acc) -> [Other | Acc].


ast_to_template(AST) ->
    ?FLAT(ast_to_template(AST, "")).

ast_to_template([], Acc) -> ?REV(Acc);
ast_to_template([{Section, _, SectionInnerAST, {Open, Close}} | Tail], Acc)
  when Section =:= section;
       Section =:= inverted ->
    Template = [Open, ast_to_template(SectionInnerAST, []), Close],
    ast_to_template(Tail, [Template | Acc]);
ast_to_template([{Section, _, TagContent, Padding} | Tail], Acc)
  when Section =:= partial;
       Section =:= dynamic ->
    ast_to_template(Tail, [?REV(Padding ++ TagContent) | Acc]);
ast_to_template([{_, _, Txt} | Tail], Acc) ->
    ast_to_template(Tail, [Txt | Acc]);
ast_to_template([{new_line, NewLine} | Tail], Acc) ->
    ast_to_template(Tail, [NewLine | Acc]);
ast_to_template([{ignore, Line} | Tail], Acc) ->
    Ignore = ast_to_template(Line, []),
    ast_to_template(Tail, [?REV(Ignore) | Acc]);
ast_to_template([List | Tail], Acc) when is_list(List) ->
    case is_string(List) of
        true -> ast_to_template(Tail, [List | Acc]);
        _ ->
            Txt = lists:map(fun ast_to_template/1, List),
            ast_to_template(Tail, [Txt | Acc])
    end.

is_string(List) when is_list(List) ->
    lists:all(fun (X) -> is_integer(X) end, ?FLAT(List)).

