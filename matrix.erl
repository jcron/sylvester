-module(matrix).

-export([default/2]).

-include_lib("eunit/include/eunit.hrl").

default(Rows, Columns) when is_integer(Rows), is_integer(Columns) ->
    [[0,0],[0,0]].

%%% EUNIT Tests
default_has_all_zeros_test() ->
    ?assert([[0,0],[0,0]] =:= default(2,2)).
    
default_needs_integers_test() ->
    ?assertError(function_clause, default('Foo', 2)),
    ?assertError(function_clause, default(2, 'Bar')).
%default_jagged_test() ->
    %?assert([[0,0],[0,0,0]] =:= default(2,3)).
