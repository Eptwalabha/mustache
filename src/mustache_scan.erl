-module(mustache_scan).

-export([tokens/2]).

-include_lib("../include/mustache.hrl").

tokens(RawTemplate, Options) ->
    Template = unicode:characters_to_list(RawTemplate),
    Tokens = tokens(Template, Options, []),
    clean(Tokens, {[], []}).

tokens([], _, [{string, String} | Acc]) ->
    ?REV([{string, ?REV(String)} | Acc]);
tokens([], _, Acc) -> ?REV(Acc);
tokens([$\r, $\n | Tail], Options, Acc) ->
    Acc2 = push_acc({new_line, "\r\n"}, Acc),
    tokens(Tail, Options, Acc2);
tokens([$\n | Tail], Options, Acc) ->
    Acc2 = push_acc({new_line, "\n"}, Acc),
    tokens(Tail, Options, Acc2);
tokens([${, ${, ${ | Tail], #{ start_tag := "{{" } = Options, Acc) ->
    Options2 = Options#{ start_tag => "{{{", end_tag => "}}}" },
    {{variable, TagName, TagContent}, Tail2} = parse_tag(Tail, Options2),
    Acc2 = push_acc({no_escape, TagName, TagContent}, Acc),
    tokens(Tail2, Options, Acc2);
tokens(Template, #{ start_tag := StartTag } = Options, Acc) ->
    case start_with(Template, StartTag) of
        {true, TagTail} ->
            case parse_tag(TagTail, Options) of
                {{delimiter, {NewStartTag, NewEndTag}, TagContent}, Tail2} ->
                    #{ start_tag := StartTag, end_tag := EndTag } = Options,
                    TagInfos = {delimiter, {StartTag, EndTag},
                                {NewStartTag, NewEndTag}, TagContent},
                    Acc2 = push_acc(TagInfos, Acc),
                    Options2 = Options#{ start_tag => NewStartTag,
                                         end_tag => NewEndTag },
                    tokens(Tail2, Options2, Acc2);
                {Tag, Tail2} ->
                    Acc2 = push_acc(Tag, Acc),
                    tokens(Tail2, Options, Acc2)
            end;
        _ ->
            [Letter | Tail] = Template,
            Acc2 = push_acc({string, [Letter]}, Acc),
            tokens(Tail, Options, Acc2)
    end.

parse_tag(Template, #{ start_tag := Start, end_tag := End }) ->
    case fetch_tag_content(Template, End, "") of
        {[$= | _] = TagContent, Tail} ->
            {NewStartTag, NewEndTag} = parse_delimiter(TagContent),
            FullTagContent = ?FLAT([Start, TagContent, End]),
            {{delimiter, {NewStartTag, NewEndTag}, FullTagContent}, Tail};
        {TagContent, Tail} ->
            FullTagContent = Start ++ TagContent ++ End,
            {TagType, TagName} = parse_tag_name(TagContent),
            {build_tag(TagType, TagName, FullTagContent), Tail}
    end.

build_tag(TypeSection, TagName, TagContent)
  when TypeSection =:= partial;
       TypeSection =:= dynamic ->
    {TypeSection, TagName, TagContent, ""};
build_tag(TypeSection, TagName, TagContent) ->
    {TypeSection, TagName, TagContent}.

parse_delimiter([$= | TagContent]) ->
    case ?REV(TagContent) of
        [$= | TagContent2] ->
            case string:lexemes(?REV(TagContent2), " \n\t") of
                [NewStartTag, NewEndTag] -> {NewStartTag, NewEndTag};
                _ -> error(wrong_delimiter)
            end;
        _ -> error(wrong_delimiter)
    end.

push_acc({string, String1}, [{string, String2} | Acc]) ->
    [{string, ?REV(String1) ++ String2} | Acc];
push_acc(Other, [{string, String} | Acc]) ->
    case string:trim(String) of
        "" -> [Other, {blank, ?REV(String)} | Acc];
        _ -> [Other, {string, ?REV(String)} | Acc]
    end;
push_acc(Other, Acc) ->
    [Other | Acc].

start_with(Tail, []) -> {true, Tail};
start_with([Same | Tail], [Same | Tail2]) -> start_with(Tail, Tail2);
start_with(_, _) -> false.

fetch_tag_content([], _, _) ->
    error(missing_end_tag);
fetch_tag_content([Letter | Tail] = TagContent, EndTag, Acc) ->
    case start_with(TagContent, EndTag) of
        {true, Tail2} -> {?REV(Acc), Tail2};
        _ -> fetch_tag_content(Tail, EndTag, [Letter | Acc])
    end.

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
parse_tag_name(Tag) -> {variable, string:trim(Tag)}.


clean([], {LineAcc, Acc}) ->
    Line = clean_line(?REV(LineAcc)),
    ?FLAT(?REV([Line | Acc]));
clean([{new_line, _} = NewLine | Tail], {LineAcc, Acc}) ->
    Line = clean_line(?REV([NewLine | LineAcc])),
    clean(Tail, {[], [Line | Acc]});
clean([Item | Tail], {LineAcc, Acc}) ->
    clean(Tail, {[Item | LineAcc], Acc}).

clean_line(Line) ->
    case has_only_comments(Line, 0) of
        true -> {ignore, Line};
        _ -> do_clean_line(Line)
    end.

has_only_comments([], Nbr) -> Nbr >= 1;
has_only_comments([{blank, _} | Tail], Nbr) ->
    has_only_comments(Tail, Nbr);
has_only_comments([{comment, _, _} | Tail], Nbr) ->
    has_only_comments(Tail, Nbr + 1);
has_only_comments(_, _) -> false.

do_clean_line([]) -> [];
do_clean_line([{new_line, _} = NewLine]) -> [NewLine];
do_clean_line(Line) ->
    case has_section_tag(Line) of
        false -> Line;
        _ ->
            {LeadingBlanks, RealContent, TrailingBlanks} = break_line(Line),
            Leading = blank_to_str(LeadingBlanks, ""),
            Trailing = blank_to_str(TrailingBlanks, ""),
            case RealContent of
                [] -> {ignore, Line};
                [{Type, Name, Content, _}]
                  when Type =:= partial;
                       Type =:= dynamic ->
                    [{Type, Name, ?FLAT([Content, Trailing]), Leading}];
                [{Type, Name, Content}]
                  when Type =:= section;
                       Type =:= inverted;
                       Type =:= end_section ->
                    FullContent = ?FLAT([Leading, Content, Trailing]),
                    [{Type, Name, FullContent}];
                [{delimiter, OldDelimiters, NewDelimiters, Content}] ->
                    FullContent = ?FLAT([Leading, Content, Trailing]),
                    [{delimiter, OldDelimiters, NewDelimiters, FullContent}];
                _ -> Line
            end
    end.

blank_to_str([], Acc) -> ?FLAT(?REV(Acc));
blank_to_str([{comment, _, Content} | Tail], Acc) ->
    blank_to_str(Tail, [Content | Acc]);
blank_to_str([{_, Content} | Tail], Acc) ->
    blank_to_str(Tail, [?REV(Content) | Acc]).

has_section_tag([]) -> false;
has_section_tag([{Type, _, _} | _])
  when Type =:= section;
       Type =:= inverted;
       Type =:= end_section;
       Type =:= comment ->
    true;
has_section_tag([{Type, _, _, _} | _])
  when Type =:= partial;
       Type =:= dynamic;
       Type =:= delimiter ->
    true;
has_section_tag([_ | Tail]) ->
    has_section_tag(Tail).

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
split_blank([{new_line, _} = NewLine | Tail], Acc) ->
    split_blank(Tail, [NewLine | Acc]);
split_blank(Tail, Acc) ->
    {?REV(Acc), Tail}.
