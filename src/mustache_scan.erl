-module(mustache_scan).

-export([tokens/2]).

-include("mustache.hrl").

tokens(RawTemplate, Options) ->
    Template = unicode:characters_to_list(RawTemplate),
    tokens(Template, Options, []).

tokens([], _, [{string, String} | Acc]) ->
    ?REV([{string, ?REV(String)} | Acc]);
tokens([], _, Acc) -> ?REV(Acc);
tokens([$\n | Tail], Options, Acc) ->
    Acc2 = push_acc(new_line, Acc),
    tokens(Tail, Options, Acc2);
tokens([${, ${, ${ | Tail], #{ start_tag := "{{" } = Options, Acc) ->
    Options2 = Options#{ start_tag => "{{{", end_tag => "}}}" },
    {variable, TagName, TagContent, Tail2} = parse_tag(Tail, Options2),
    Acc2 = push_acc({no_escape, TagName, TagContent}, Acc),
    tokens(Tail2, Options, Acc2);
tokens(Template, #{ start_tag := StartTag } = Options, Acc) ->
    case start_with(Template, StartTag) of
        {true, TagTail} ->
            case parse_tag(TagTail, Options) of
                {delimiter, {NewStartTag, NewEndTag}, TagContent, Tail2} ->
                    #{ start_tag := StartTag, end_tag := EndTag } = Options,
                    TagInfos = {delimiter, {StartTag, EndTag},
                                {NewStartTag, NewEndTag}, TagContent},
                    Acc2 = push_acc(TagInfos, Acc),
                    Options2 = Options#{ start_tag => NewStartTag,
                                         end_tag => NewEndTag },
                    tokens(Tail2, Options2, Acc2);
                {TagType, TagName, TagContent, Tail2} ->
                    Acc2 = push_acc({TagType, TagName, TagContent}, Acc),
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
            {delimiter, {NewStartTag, NewEndTag}, FullTagContent, Tail};
        {TagContent, Tail} ->
            FullTagContent = Start ++ TagContent ++ End,
            case parse_tag_name(TagContent) of
                {TagType, TagName} ->
                    {TagType, TagName, FullTagContent, Tail};
                TagName ->
                    {variable, TagName, FullTagContent, Tail}
            end
    end.

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
parse_tag_name(Tag) -> string:trim(Tag).

