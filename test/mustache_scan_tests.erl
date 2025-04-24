-module(mustache_scan_tests).

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
    Expected = [{new_line, "\n"},
                {string, "a line"}, {new_line, "\n"},
                {string, "another line"}, {new_line, "\n"}],
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
    Expected = [{blank, " \t  "}, {new_line, "\n"},
                {string, "  hello  "}, {new_line, "\n"},
                {variable, "world", "{{world}}"},
                {blank, "   "}, {new_line, "\n"},
                {string, "  \t!  "}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_section_test() ->
    Template = "{{# section }}hello{{/section  }}",
    Expected = [{section, "section", "{{# section }}"},
                {string, "hello"},
                {end_section, "section", "{{/section  }}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_inverted_section_test() ->
    Template = "{{^ inverted-section }}\ninverted\n{{/inverted-section  }}",
    Expected = [{inverted, "inverted-section", "{{^ inverted-section }}\n"},
                {string, "inverted"}, {new_line, "\n"},
                {end_section, "inverted-section", "{{/inverted-section  }}"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_partial_test() ->
    Template = "{{> the-partial }}",
    Expected = [{partial, "the-partial", "{{> the-partial }}", ""}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_dynamic_name_test() ->
    Template = "{{> * the-dynamic-name }}\n{{>*dynamic}}",
    Expected = [{dynamic, "the-dynamic-name",
                 "{{> * the-dynamic-name }}\n", ""},
                {dynamic, "dynamic", "{{>*dynamic}}", ""}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_comment_test() ->
    Template = "{{! super\nmulti-line\ncomment }}\n{{!inline comment}}",
    Expected = [{ignore, [{comment, "super\nmulti-line\ncomment",
                          "{{! super\nmulti-line\ncomment }}"},
                          {new_line, "\n"}]},
                {ignore, [{comment, "inline comment",
                           "{{!inline comment}}"}]}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_comment_with_blanks_test() ->
    Template = "  {{! comment }}  \n  \n",
    Expected = [{ignore, [{blank, "  "},
                          {comment, "comment", "{{! comment }}"},
                          {blank, "  "},
                          {new_line, "\n"}]},
                {blank, "  "}, {new_line, "\n"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize_comment_with_section_test() ->
    Template = "{{# section }}  {{! comment }}  \ncontent\n{{/ section }}\n",
    Expected = [{section, "section", "{{# section }}  {{! comment }}  \n"},
                {string, "content"}, {new_line, "\n"},
                {end_section, "section", "{{/ section }}\n"}],
    ?assertEqual(Expected, tokenize(Template)).

tokenize(Template) ->
    Options = #{ start_tag => "{{", end_tag => "}}" },
    mustache_scan:tokens(Template, Options).

