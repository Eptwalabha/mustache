-module(mustache_tests).

-include_lib("eunit/include/eunit.hrl").


render_empty_template_test() ->
    ?assertEqual("", mustache:render("")).

render_test() ->
    ?assertEqual("", mustache:render("{{mustache}}")).

render_substitute_variable_test() ->
    Map = #{ mustache => "toto" },
    ?assertEqual("toto", mustache:render("{{mustache}}", Map)).

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
                  {Template, mustache:render(Template, #{ var => "süper"})})
     || Template <- Templates].

render_all_variables_test() ->
    Template = "{{var1}} {{var2}} {{var3}} {{var1}}",
    Map = #{ var1 => "toto", var2 => "tata"},
    ?assertEqual("toto tata  toto", mustache:render(Template, Map)).

render_type_test() ->
    Template = "{{atom}}, {{int}}, {{float}}, {{string}}, {{binary}}, {{bool}}",
    Map = #{ atom => atom,
             int => 10,
             float => 10.01,
             string => "strïng",
             binary => <<"bïnary"/utf8>>,
             bool => true },
    ?assertEqual("atom, 10, 10.01, strïng, bïnary, true",
                 mustache:render(Template, Map)).

render_unicode_test() ->
    Template = "the city {{ city }} is located on the island of Oʻahu",
    Map = #{ city => "Māʻili" },
    ?assertEqual("the city Māʻili is located on the island of Oʻahu",
                 mustache:render(Template, Map)).

render_escape_html_test() ->
    Template = "{{injection}}",
    ?assertEqual("&lt;script&gt;",
                 mustache:render(Template, #{ injection => "<script>" })),
    ?assertEqual("&lt;b&gt;test&lt;/b&gt;",
                 mustache:render(Template, #{ injection => "<b>test</b>" })),
    ?assertEqual("&amp;&quot;&apos;",
                 mustache:render(Template, #{ injection => "&\"'" })),
    ?assertEqual("",
                 mustache:render(Template, #{ injection => null })).

render_does_not_escape_html_test() ->
    Template = "{{{html}}}",
    ?assertEqual("<script>",
                 mustache:render(Template, #{ html => "<script>" })),
    ?assertEqual("<b>test</b>",
                 mustache:render(Template, #{ html => "<b>test</b>" })),
    ?assertEqual("&\"'",
                 mustache:render(Template, #{ html => "&\"'" })),
    ?assertEqual("",
                 mustache:render(Template, #{ html => null })).

render_with_ampersand_does_not_escape_html_test() ->
    Template = "{{& html }}",
    ?assertEqual("<script>",
                 mustache:render(Template, #{ html => "<script>" })),
    ?assertEqual("<b>test</b>",
                 mustache:render(Template, #{ html => "<b>test</b>" })),
    ?assertEqual("&\"'",
                 mustache:render(Template, #{ html => "&\"'" })),
    ?assertEqual("",
                 mustache:render(Template, #{ html => null })).

render_do_not_render_sections_test() ->
    Template = "{{#section}}won't show{{/section}}",
    ?assertEqual("", mustache:render(Template)),
    ?assertEqual("", mustache:render(Template, #{ section => false })),
    ?assertEqual("", mustache:render(Template, #{ section => null })),
    ?assertEqual("", mustache:render(Template, #{ section => [] })),
    ?assertEqual("", mustache:render(Template, #{ section => <<"">> })),
    ?assertEqual("", mustache:render(Template, #{ section => 0 })).

render_section_if_true_test() ->
    Template = "{{#section}}hello {{name}}{{/section}}",
    Map = #{ section => true,
             name => "world" },
    ?assertEqual("hello world", mustache:render(Template, Map)).

render_section_with_non_empty_array_test() ->
    Template = "list:{{#list}} {{.}}{{/list}}",
    ?assertEqual("list: 1 2 3",
                 mustache:render(Template, #{ list => [1, 2, 3] })),
    ?assertEqual("list: 1.2 3.4 5.6",
                 mustache:render(Template, #{ list => [1.2, 3.4, 5.6] })),
    ?assertEqual("list: 97 98 99",
                 mustache:render(Template, #{ list => "abc" })),
    ?assertEqual("list: a b c",
                 mustache:render(Template, #{ list => ["a", "b", "c"] })),
    ?assertEqual("list: a b c",
                 mustache:render(Template, #{ list => [a, b, c] })).

render_section_with_other_than_array_test() ->
    Template = "item: {{#section}}{{.}}{{/section}}",
    ?assertEqual("item: 123",
                 mustache:render(Template, #{ section => 123 })),
    ?assertEqual("item: 123.456",
                 mustache:render(Template, #{ section => 123.456 })),
    ?assertEqual("item: atom",
                 mustache:render(Template, #{ section => atom })),
    ?assertEqual("item: true",
                 mustache:render(Template, #{ section => true })),
    ?assertEqual("item: Häns",
                 mustache:render(Template, #{ section => <<"Häns"/utf8>> })).

render_section_with_array_test() ->
    Template = "users:\n{{#users}}- {{.}}\n{{/users}}",
    Map = #{ users => ["Huey", "Dewey", "Louie"] },
    Expected = "users:\n"
               "- Huey\n"
               "- Dewey\n"
               "- Louie\n",
    ?assertEqual(Expected, mustache:render(Template, Map)).

render_section_pass_context_test() ->
    Template = "ducks:"
               "{{#ducks}}"
                    " {{duck_name}}"
               "{{/ducks}}",
    Map = #{ duck_name => "Donald",
             ducks => [#{ name => "Pluto" },
                       #{ duck_name => "Huey" },
                       #{ duck_name => "Dewey" },
                       #{ duck_name => "Louie" }] },
    ?assertEqual("ducks: Donald Huey Dewey Louie",
                 mustache:render(Template, Map)).

render_section_with_html_test() ->
    Template = "<ul>{{#list}}<li>{{item}}: {{{item}}}</li>{{/list}}</ul>",
    Map = #{ list => [#{ item => "<b>item1</b>" },
                      #{ item => "<i>item2</i>" },
                      #{ item => "<strong>item3</strong>" }]},
    Expected = "<ul>"
                   "<li>&lt;b&gt;item1&lt;/b&gt;: <b>item1</b></li>"
                   "<li>&lt;i&gt;item2&lt;/i&gt;: <i>item2</i></li>"
                   "<li>&lt;strong&gt;item3&lt;/strong&gt;: "
                       "<strong>item3</strong></li>"
               "</ul>",
    ?assertEqual(Expected, mustache:render(Template, Map)).

render_section_with_unicode_test() ->
    Template = "cost (¥): {{#unicode}}ランプ {{ cost }}-{{/unicode}}",
    Map = #{ unicode => true,
             cost => 10.99 },
    ?assertEqual("cost (¥): ランプ 10.99-", mustache:render(Template, Map)).

render_section_with_lambda_test() ->
    Lambda = fun (Template, Render) ->
                     "<b>" ++ Render(Template) ++ "</b>"
             end,
    Template = "ラムダ: {{#lambda}}hellö {{who}}{{/lambda}}",
    Map = #{ lambda => Lambda,
             who => "wōrld" },
    ?assertEqual("ラムダ: <b>hellö wōrld</b>",
                 mustache:render(Template, Map)).

render_section_substitution_with_lambda_test() ->
    Lambda = fun () -> "<Hellö>" end,
    Template = "ラムダ: {{lambda}} {{&lambda}}",
    Map = #{ lambda => Lambda },
    ?assertEqual("ラムダ: &lt;Hellö&gt; <Hellö>",
                 mustache:render(Template, Map)).

render_section_substitution_with_lambda_are_compiled_test() ->
    Lambda = fun () -> "Hëllo <{{who}}>" end,
    Template = "{{&lambda}}",
    Map = #{ lambda => Lambda,
             who => <<"wōrld"/utf8>> },
    ?assertEqual("Hëllo <wōrld>",
                 mustache:render(Template, Map)).

render_section_with_no_endtag_test() ->
    Template = "Dwarves: {{#dwarves}}{{.}},",
    Map = #{ dwarves => ["Doc", "Happy", "Bashful", "Grumpy",
                         "Sleepy", "Sneezy", "Dopey"] },
    Expected = {missing_end_section, "dwarves"},
    ?assertError(Expected, mustache:render(Template, Map)).

render_ignore_comments_test() ->
    Template = "Hello {{! ignore me}}world{{!ignore}}",
    Map = #{ '!ignore' => "won't be shown" },
    ?assertEqual("Hello world", mustache:render(Template, Map)).

render_inverted_section_test() ->
    Template = "abc{{^var}} def {{/var}}ghi",
    ?assertEqual("abcghi", mustache:render(Template, #{ var => "0123" })),
    ?assertEqual("abcghi", mustache:render(Template, #{ var => true })),
    ?assertEqual("abc def ghi", mustache:render(Template, #{var => "" })),
    ?assertEqual("abc def ghi", mustache:render(Template, #{var => null })),
    ?assertEqual("abc def ghi", mustache:render(Template, #{var => false })).

render_partial_section_test() ->
    Template = "{{#names}}{{>test}}{{/names}}",
    Partials = #{ "test" => "my name is {{name}}\n" },
    Map = #{ names => [#{ name => "Huey" },
                       #{ name => "Dewey" },
                       #{ name => "Louie" }] },
    ?assertEqual("my name is Huey\n"
                 "my name is Dewey\n"
                 "my name is Louie\n",
                 mustache:render(Template, Map, Partials)).

render_partial_section_not_defined_test() ->
    Template = "{{>does-not-exists}}",
    ?assertEqual("", mustache:render(Template)).

-ifdef(capturedOutput).
main_only_template_test() ->
    mustache:main(["Hello {{name}}!"]),
    ?assertEqual("Hello !\n", ?capturedOutput).
-endif.

render_multiple_sections_test() ->
    Template = "{{#items}}- {{name}}\n{{/items}}"
               "{{#list}}- {{.}}\n{{/list}}",
    Map = #{ items => [#{ name => "item1" },
                       #{ name => "item2" },
                       #{ name => "item3" }],
             list => ["itemA", "itemB", "itemC"] },
    ?assertEqual("- item1\n- item2\n- item3\n"
                 "- itemA\n- itemB\n- itemC\n",
                 mustache:render(Template, Map)).

render_allows_proplists_test() ->
    Template = "\n"
               "Hello {{#friend}}{{name}} ({{surname}}){{/friend}}"
               "{{#elements}} {{.}}{{/elements}} "
               "{{something}} - "
               "{{#map.proplist}}{{.}}, {{/map.proplist}}",
    Proplist = [{friend, [{name, "John"}, {surname, "Joe"}]},
                {"elements", ["Peter", "Loïs"]},
                {something, 'else'},
                {map, #{ proplist => ["a", <<"b">>] }}],
    ?assertEqual("\nHello John (Joe) Peter Loïs else - a, b, ",
                 mustache:render(Template, Proplist)).

render_allows_mix_chardata_template_test() ->
    Template = ["Hello ", <<"how">>, [[[" "]]],
                ["{{", <<"test">>, ["}}"]],
                <<" yöu"/utf8>>, [""], <<"?">>],
    ?assertEqual("Hello how are yöu?",
                 mustache:render(Template, #{ test => <<"are">> })).

render_dot_notation_test() ->
    Template = "{{# a.b.c }}{{a.b.v}}{{/ a.b.c }}",
    Map = #{ a => #{ b => #{ c => true, v => <<"Hello world">> } } },
    ?assertEqual("Hello world", mustache:render(Template, Map)).

render_dot_notation_not_found_test() ->
    Template = "{{# a.b.c }}{{a.b}}{{/ a.b.c }}",
    Map = #{ a => #{ b => 123 } },
    ?assertEqual("", mustache:render(Template, Map)).

render_dot_for_single_param_test() ->
    Template = "foo {{.}}",
    ?assertEqual("foo bar", mustache:render(Template, "bar")).
