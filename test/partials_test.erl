%% --------------------------------------------------
%% DO NOT EDIT THIS FILE
%% It was generated automatically from the following specs
%% ./mustache-spec/specs/partials.json
%% Fetched from this repository
%% https://github.com/mustache/spec
%%
%% date: 2022-10-20 19:31:19
%% @version v0.0.0
%% @reference Eptwalabha, https://github.com/Eptwalabha.
%%
%% @doc Partial tags are used to expand an external template into the current
%% template.
%%
%% The tag's content MUST be a non-whitespace character sequence NOT containing
%% the current closing delimiter.
%%
%% This tag's content names the partial to inject. Set Delimiter tags MUST NOT
%% affect the parsing of a partial. The partial MUST be rendered against the
%% context stack local to the tag. If the named partial cannot be found, the
%% empty string SHOULD be used instead, as in interpolations.
%%
%% Partial tags SHOULD be treated as standalone when appropriate. If this tag
%% is used standalone, any whitespace preceding the tag should treated as
%% indentation, and prepended to each line of the partial before rendering.
%%
%% @end

-module(partials_test).

-include_lib("eunit/include/eunit.hrl").

-define(TO_LIST(Something), unicode:characters_to_list(Something)).

%% @doc The greater-than operator should expand to the named partial.
basic_behavior_test() ->
    Template = <<"\"{{>text}}\"">>,
    Data = #{},
    Partials = #{ text => <<"from partial">> },
    Expected = <<"\"from partial\"">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc The empty string should be used when the named partial is not found.
failed_lookup_test() ->
    Template = <<"\"{{>text}}\"">>,
    Data = #{},
    Partials = #{},
    Expected = <<"\"\"">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc The greater-than operator should operate within the current context.
context_test() ->
    Template = <<"\"{{>partial}}\"">>,
    Data = #{text => <<"content">>},
    Partials = #{ partial => <<"*{{text}}*">> },
    Expected = <<"\"*content*\"">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc The greater-than operator should properly recurse.
recursion_test() ->
    Template = <<"{{>node}}">>,
    Data = #{ content => <<"X">>,
              nodes => [#{ content => <<"Y">>,
                           nodes => [] }]},
    Partials = #{ node => <<"{{content}}<{{#nodes}}{{>node}}{{/nodes}}>">> },
    Expected = <<"X<Y<>>">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc The greater-than operator should not alter surrounding whitespace.
surrounding_whitespace_test() ->
    Template = <<"| {{>partial}} |">>,
    Data = #{},
    Partials = #{ partial => <<"\t|\t">> },
    Expected = <<"| \t|\t |">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc Whitespace should be left untouched.
inline_indentation_test() ->
    Template = <<"  {{data}}  {{> partial}}\n">>,
    Data = #{data => <<"|">>},
    Partials = #{ partial => <<">\n>">> },
    Expected = <<"  |  >\n>\n">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc "\r\n" should be considered a newline for standalone tags.
standalone_line_endings_test() ->
    Template = <<"|\r\n{{>partial}}\r\n|">>,
    Data = #{},
    Partials = #{ partial => <<">">> },
    Expected = <<"|\r\n>|">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc Standalone tags should not require a newline to precede them.
standalone_without_previous_line_test() ->
    Template = <<"  {{>partial}}\n>">>,
    Data = #{},
    Partials = #{ partial => <<">\n>">> },
    Expected = <<"  >\n  >>">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc Standalone tags should not require a newline to follow them.
standalone_without_newline_test() ->
    Template = <<">\n  {{>partial}}">>,
    Data = #{},
    Partials = #{ partial => <<">\n>">> },
    Expected = <<">\n  >\n  >">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc Each line of the partial should be indented before rendering.
standalone_indentation_test() ->
    Template = <<"\\\n {{>partial}}\n/\n">>,
    Data = #{content => <<"<\n->">>},
    Partials = #{ partial => <<"|\n{{{content}}}\n|\n">> },
    Expected = <<"\\\n |\n <\n->\n |\n/\n">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).

%% @doc Superfluous in-tag whitespace should be ignored.
padding_whitespace_test() ->
    Template = <<"|{{> partial }}|">>,
    Data = #{boolean => true},
    Partials = #{ partial => <<"[]">> },
    Expected = <<"|[]|">>,
    ?assertEqual(?TO_LIST(Expected),
                 mustache:render(Template, Data, Partials)).
