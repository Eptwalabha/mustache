-module(specs_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VAL(Key, Proplists), proplists:get_value(Key, Proplists)).
-define(VAL(Key, Proplists, Default),
        proplists:get_value(Key, Proplists, Default)).
-define(UTL(Something), unicode:characters_to_list(Something)).

specs_test_() ->
    [generate_suite(SpecFile)
     || SpecFile <- filelib:wildcard("*.json", "test/specs"),
        is_spec_handeled(SpecFile)].

is_spec_handeled("~inheritance.json") -> false; % not implemented
is_spec_handeled("~lambdas.json") -> true; % lambda
is_spec_handeled(_) -> true.

generate_suite(File) ->
    {ok, Json} = file:read_file(["test/specs/", File]),
    Data = jsx:decode(Json, [{labels, atom}]),
    {ok, Suite} = maps:find(tests, Data),
    lists:map(fun generate_case/1, Suite).

generate_case(TestSpec) ->
    Name = maps:get(name, TestSpec),
    Desc = maps:get(desc, TestSpec),
    Data = extract_data(maps:get(data, TestSpec)),
    Partials = maps:get(partials, TestSpec, nopartials),
    Template = maps:get(template, TestSpec, <<"">>),
    {unicode:characters_to_binary([Name, " : ", Desc]),
     ?_assertEqual(maps:get(expected, TestSpec),
                   render(Template, Data, Partials))}.

render(Template, Data, nopartials) ->
    unicode:characters_to_binary(mustache:render(Template, Data));
render(Template, Data, Partials) ->
    unicode:characters_to_binary(mustache:render(Template, Data, Partials)).

extract_data(#{ lambda := Lambdas } = Data) ->
    LambdaStr = maps:get(erlang, Lambdas),
    Data#{ lambda => parse_fun_expr(binary_to_list(LambdaStr)) };
extract_data(Data) ->
    Data.

parse_fun_expr(S) ->
  {ok, Ts, _} = erl_scan:string(S),
  {ok, Exprs} = erl_parse:parse_exprs(Ts),
  {value, Fun, _} = erl_eval:exprs(Exprs, []),
  Fun.
