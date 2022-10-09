-module(mustache_test).

-include_lib("eunit/include/eunit.hrl").


render_test() ->
    ?assertEqual("", mustache:render("{{mustache}}")).

render_substitute_variable_test() ->
    ?assertEqual("toto", mustache:render("{{mustache}}", [{mustache, "toto"}])).

render_variables_test() ->
    Templates = ["{{var}}",
                 "{{ var }}",
                 "{{       var       }}",
                 "{{var       }}",
                 "{{       var}}",
                 "{{{var}}}",
                 "{{{ var }}}",
                 "{{{       var       }}}",
                 "{{{var       }}}",
                 "{{{       var}}}"],
    [?assertEqual({Template, "süper"},
                  {Template, mustache:render(Template, [{var, "süper"}])})
     || Template <- Templates].

render_all_variables_test() ->
    Template = "{{var1}} {{var2}} {{var3}} {{var1}}",
    ?assertEqual("toto tata  toto",
                 mustache:render(Template, [{var1, "toto"}, {var2, "tata"}])).

render_missing_end_delimiter_test() ->
    Template = "Hello {{name",
    ?assertEqual("Hello {{name", mustache:render(Template, [{name, "Tom"}])).

render_broken_tag_test() ->
    Template = "I am become {{name}, the {{job}}",
    Params = [{name, "Death"},
              {job, "destroyer of worlds"}],
    ?assertEqual("I am become {{name}, the destroyer of worlds",
                 mustache:render(Template, Params)).

render_type_test() ->
    Template = "{{atom}}, {{int}}, {{float}}, {{string}}, {{binary}}, {{bool}}",
    Params = [{atom, atom}, {int, 10}, {float, 10.01},
              {string, "strïng"}, {binary, <<"bïnary"/utf8>>}, {bool, false}],
    ?assertEqual("atom, 10, 10.01, strïng, bïnary, false",
                 mustache:render(Template, Params)).

render_unicode_test() ->
    Template = "the city {{ city }} is located on the island of Oʻahu",
    ?assertEqual("the city Māʻili is located on the island of Oʻahu",
                 mustache:render(Template, [{city, "Māʻili"}])).

render_escape_html_test() ->
    Template = "{{injection}}",
    ?assertEqual("&lt;script&gt;",
                 mustache:render(Template, [{injection, "<script>"}])),
    ?assertEqual("&lt;b&gt;test&lt;/b&gt;",
                 mustache:render(Template, [{injection, "<b>test</b>"}])).

render_missing_end_delimiter_escape_test() ->
    Template = "Hello {{{name",
    ?assertEqual("Hello {{{name", mustache:render(Template, [{name, "Tom"}])).

render_broken_escape_tag_test() ->
    Template = "I am become {{{name}}, the {{{job}}}",
    Params = [{name, "Death"},
              {job, "destroyer of worlds"}],
    ?assertEqual("I am become {{{name}}, the destroyer of worlds",
                 mustache:render(Template, Params)).

render_does_not_escape_html_test() ->
    Template = "{{{html}}}",
    ?assertEqual("<script>",
                 mustache:render(Template, [{html, "<script>"}])),
    ?assertEqual("<b>test</b>",
                 mustache:render(Template, [{html, "<b>test</b>"}])).

render_do_not_render_sections_test() ->
    Template = "{{#section}}won't show{{/section}}",
    ?assertEqual("", mustache:render(Template)),
    ?assertEqual("", mustache:render(Template, [{section, false}])),
    ?assertEqual("", mustache:render(Template, [{section, []}])).

render_section_if_true_test() ->
    Template = "{{#section}}hello {{name}}{{/section}}",
    Params = [{section, true}, {name, "world"}],
    ?assertEqual("hello world", mustache:render(Template, Params)).

render_section_with_non_empty_array_test() ->
    Template = "list:{{#list}} {{.}}{{/list}}",
    ?assertEqual("list: 1 2 3",
                 mustache:render(Template, [{list, [1, 2, 3]}])),
    ?assertEqual("list: 1.2 3.4 5.6",
                 mustache:render(Template, [{list, [1.2, 3.4, 5.6]}])),
    ?assertEqual("list: 97 98 99",
                 mustache:render(Template, [{list, "abc"}])),
    ?assertEqual("list: one two three",
                 mustache:render(Template, [{list, ["one", "two", "three"]}])),
    ?assertEqual("list: a b c",
                 mustache:render(Template, [{list, [a, b, c]}])).

render_section_with_other_than_array_test() ->
    Template = "item: {{#section}}{{.}}{{/section}}",
    ?assertEqual("item: 123",
                 mustache:render(Template, [{section, 123}])),
    ?assertEqual("item: 123.456",
                 mustache:render(Template, [{section, 123.456}])),
    ?assertEqual("item: atom",
                 mustache:render(Template, [{section, atom}])),
    ?assertEqual("item: true",
                 mustache:render(Template, [{section, true}])),
    ?assertEqual("item: Häns",
                 mustache:render(Template, [{section, <<"Häns"/utf8>>}])).

render_section_with_array_test() ->
    Template = "users:\n{{#users}}- {{.}}\n{{/users}}",
    Params = [{users, ["Huey", "Dewey", "Louie"]}],
    Expected = "users:\n"
               "- Huey\n"
               "- Dewey\n"
               "- Louie\n",
    ?assertEqual(Expected, mustache:render(Template, Params)).

render_section_pass_context_test() ->
    Template = "ducks:"
               "{{#ducks}}"
                    " {{name}}"
               "{{/ducks}}",
    Params = [{name, "Donald"},
              {ducks, [[],
                       [{name, "Huey"}],
                       [{name, "Dewey"}],
                       [{name, "Louie"}]]}],
    ?assertEqual("ducks: Donald Huey Dewey Louie", mustache:render(Template, Params)).

render_section_with_html_test() ->
    Template = "<ul>{{#list}}<li>{{item}}: {{{item}}}</li>{{/list}}</ul>",
    Params = [{list, [[{item, "<b>item1</b>"}],
                      [{item, "<i>item2</i>"}],
                      [{item, "<strong>item3</strong>"}]]}],
    Expected = "<ul>"
                   "<li>&lt;b&gt;item1&lt;/b&gt;: <b>item1</b></li>"
                   "<li>&lt;i&gt;item2&lt;/i&gt;: <i>item2</i></li>"
                   "<li>&lt;strong&gt;item3&lt;/strong&gt;: <strong>item3</strong></li>"
               "</ul>",
    ?assertEqual(Expected, mustache:render(Template, Params)).

render_section_with_unicode_test() ->
    Template = "cost (¥): {{#unicode}}ランプ {{ cost }}-{{/unicode}}",
    Params = [{unicode, true},
              {cost, 10.99}],
    ?assertEqual("cost (¥): ランプ 10.99-", mustache:render(Template, Params)).

render_section_with_lambda_test() ->
    Lambda = fun (Template, Render) ->
                     "<b>" ++ Render(Template) ++ "</b>"
             end,
    Template = "ラムダ: {{#lambda}}hellö {{who}}{{/lambda}}",
    Params = [{lambda, Lambda},
              {who, "wōrld"}],
    ?assertEqual("ラムダ: <b>hellö wōrld</b>",
                 mustache:render(Template, Params)).

render_section_with_no_endtag_test() ->
    Template = "Dwarves: {{#dwarves}}{{.}},",
    Params = [{dwarves, ["Doc", "Happy", "Bashful", "Grumpy",
                         "Sleepy", "Sneezy", "Dopey"]}],
    Expected = "Dwarves: Doc,Happy,Bashful,Grumpy,Sleepy,Sneezy,Dopey,",
    ?assertEqual(Expected, mustache:render(Template, Params)).

render_ignore_comments_test() ->
    Template = "Hello {{! ignore me}}world{{!ignore}}",
    Params = [{'!ignore', "won't be shown"}],
    ?assertEqual("Hello world", mustache:render(Template, Params)).

render_inverted_section_test() ->
    Template = "abc{{^var}} def {{/var}}ghi",
    ?assertEqual("abcghi", mustache:render(Template, [{var, "0123"}])),
    ?assertEqual("abcghi", mustache:render(Template, [{var, true}])),
    ?assertEqual("abc def ghi", mustache:render(Template, [{var, ""}])),
    ?assertEqual("abc def ghi", mustache:render(Template, [{var, false}])).

render_partial_section_test() ->
    Template = "{{#names}}{{>test}}{{/names}}",
    Params = [{test, "my name is {{name}}\n"},
              {names, [[{name, "Huey"}],
                       [{name, "Dewey"}],
                       [{name, "Louie"}]]}],
    ?assertEqual("my name is Huey\n"
                 "my name is Dewey\n"
                 "my name is Louie\n",
                 mustache:render(Template, Params)).

render_partial_section_not_defined_test() ->
    Template = "{{>does-not-exists}}",
    ?assertEqual("", mustache:render(Template)).

main_only_template_test() ->
    mustache:main(["Hello {{name}}!"]),
    ?assertEqual("Hello !\n", ?capturedOutput).
