-module(mustache_ast).

-export([build/1]).
-export([ast_to_template/1]).

-include("mustache.hrl").

build(Tokens) ->
    CleanedTokens = clean_tokens(Tokens),
    build_ast(CleanedTokens).

clean_tokens(Tokens) ->
    clean(Tokens, {[], []}).

clean([], {LineAcc, Acc}) ->
    Line = clean_line(?REV(LineAcc)),
    ?FLAT(?REV([Line | Acc]));
clean([new_line | Tail], {LineAcc, Acc}) ->
    Line = clean_line(?REV([new_line | LineAcc])),
    clean(Tail, {[], [Line | Acc]});
clean([Item | Tail], {LineAcc, Acc}) ->
    clean(Tail, {[Item | LineAcc], Acc}).

clean_line([]) -> [];
clean_line([new_line]) -> [new_line];
clean_line(Line) ->
    case has_string(Line) of
        true -> Line;
        _ ->
            {_LeadingBlanks, RealContent, _TrailingBlanks} = break_line(Line),
            case RealContent of
                [] -> [];
                [{delimiter, _, _, _} = Delimiter] -> [Delimiter];
                [{section, _, _} = Section] -> [Section];
                [{inverted, _, _} = Section] -> [Section];
                [{end_section, _, _} = Section] -> [Section];
                [{partial, _, _} = Partial] -> [Partial];
                [{dynamic, _, _} = Partial] -> [Partial];
                _ -> Line
            end
    end.

has_string(Line) ->
    lists:any(fun ({string, _}) -> true; (_) -> false end, Line).

break_line(Line) ->
    {LeadingBlanks, Tail} = split_blank(Line, []),
    {TrailingBlanks, Tail2} = split_blank(?REV(Tail), []),
    {LeadingBlanks, ?REV(Tail2), ?REV(TrailingBlanks)}.

split_blank([], Acc) ->
    {?REV(Acc), []};
split_blank([{blank, _} = Blank | Tail], Acc) ->
    split_blank(Tail, [Blank | Acc]);
split_blank([{comment, _, _} = Comment | Tail], Acc) ->
    split_blank(Tail, [Comment | Acc]);
split_blank([new_line | Tail], Acc) ->
    split_blank(Tail, [new_line | Acc]);
split_blank(Tail, Acc) ->
    {?REV(Acc), Tail}.

build_ast(Tokens) ->
    {AST, _, _} = ast(Tokens, null, []),
    AST.

ast([], null, Acc) -> {?REV(Acc), "", []};
ast([], SectionName, _) ->
    error({missing_end_section, SectionName});
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


ast_to_template(AST) ->
    ?FLAT(ast_to_template(AST, "")).

ast_to_template([], Acc) -> ?REV(Acc);
ast_to_template([{Section, _, SectionInnerAST, {Open, Close}} | Tail], Acc)
  when Section =:= section;
       Section =:= inverted ->
    Template = [Open, ast_to_template(SectionInnerAST, []), Close],
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

