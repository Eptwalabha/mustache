-module(mustache_ast_test).

-include_lib("eunit/include/eunit.hrl").

ast_to_template_full_test() ->
    Template = <<"toto {{! comment }}  \n"
                 "  {{#section  }}  {{! comment }}\n"
                 "\t  {{>* dynamic }} {{! comment }}\n"
                 "single line\n"
                 "{{!comment1}} {{!comment2}} {{!comment3}}\n"
                 "test öne two {{value}}\n"
                 "{{/section}}"/utf8>>,
    TokenizerOptions = #{ start_tag => "{{", end_tag => "}}" },
    Tokens = mustache_scan:tokens(Template, TokenizerOptions),
    Ast = mustache_ast:build(Tokens),
    ?assertEqual(unicode:characters_to_list(Template),
                 mustache_ast:ast_to_template(Ast)).
