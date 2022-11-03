-module(mustache_scan_test).

-include_lib("eunit/include/eunit.hrl").

tokenize_empty_test() ->
    ?assertEqual([], tokenize("")).

tokenize_tag_test() ->
    Template = "{{tag}}",
    Expected = [{variable, "tag", "{{tag}}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_broken_end_tag_test() ->
    Template = "{{tag",
    ?assertError(missing_end_tag, tokenize(Template)).

tokenize_broken_triple_tag_test() ->
    Template = "{{{tag}}",
    ?assertError(missing_end_tag, tokenize(Template)).

tokenize_new_line_test() ->
    Template = "\na line\nanother line\n",
    Expected = [new_line,
                {string, "a line"}, new_line,
                {string, "another line"}, new_line],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_delimiter_test() ->
    Template = "{{tag}}{{=|   |=}}|tag||={{ }}=||tag|",
    Expected = [{variable, "tag", "{{tag}}"},
                {delimiter, {"{{", "}}"}, {"|", "|"}, "{{=|   |=}}"},
                {variable, "tag", "|tag|"},
                {delimiter, {"|", "|"}, {"{{", "}}"}, "|={{ }}=|"},
                {string, "|tag|"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_broken_delimiters_test() ->
    Template = "{{=abc=}}",
    ?assertError(wrong_delimiter, tokenize(Template)).

tokenize_badly_formed_delimiter_test() ->
    Template = "{{=| |}}",
    ?assertError(wrong_delimiter, tokenize(Template)).

tokenize_no_escape_tag_test() ->
    Template = "{{{tag}}}{{=| |=}}|&tag|{{{tag}}}",
    Expected = [{no_escape, "tag", "{{{tag}}}"},
                {delimiter, {"{{", "}}"}, {"|", "|"}, "{{=| |=}}"},
                {no_escape, "tag", "|&tag|"},
                {string, "{{{tag}}}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_blank_test() ->
    Template = " \t  \n  hello  \n{{world}}   \n  \t!  ",
    Expected = [{blank, " \t  "}, new_line,
                {string, "  hello  "}, new_line,
                {variable, "world", "{{world}}"}, {blank, "   "}, new_line,
                {string, "  \t!  "}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_section_test() ->
    Template = "{{# section }}hello{{/section  }}",
    Expected = [{section, "section", "{{# section }}"},
                {string, "hello"},
                {end_section, "section", "{{/section  }}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_inverted_section_test() ->
    Template = "{{^ inverted-section }}\n{{/inverted-section  }}",
    Expected = [{inverted, "inverted-section", "{{^ inverted-section }}"},
                new_line,
                {end_section, "inverted-section", "{{/inverted-section  }}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_partial_test() ->
    Template = "{{> the-partial }}",
    Expected = [{partial, "the-partial", "{{> the-partial }}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_dynamic_name_test() ->
    Template = "{{> * the-dynamic-name }}\n{{>*dynamic}}",
    Expected = [{dynamic, "the-dynamic-name",
                 "{{> * the-dynamic-name }}"},
                new_line,
                {dynamic, "dynamic", "{{>*dynamic}}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_comment_test() ->
    Template = "{{! super\nmulti-line\ncomment }}\n{{!inline comment}}",
    Expected = [{comment, "super\nmulti-line\ncomment",
                 "{{! super\nmulti-line\ncomment }}"},
                new_line,
                {comment, "inline comment", "{{!inline comment}}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize(Template) ->
    Options = #{ start_tag => "{{", end_tag => "}}" },
    mustache_scan:tokens(Template, Options).

