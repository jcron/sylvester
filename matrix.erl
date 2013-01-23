-module(matrix).

-export([default/2,
         print/0]).

-include_lib("eunit/include/eunit.hrl").

default(Rows, Columns) when is_integer(Rows), is_integer(Columns) ->
    build_matrix(Rows, Columns).

build_list(Size) ->
    build_list(Size, []).

build_list(0, List) ->
    List;
build_list(Size, List) ->
    build_list(Size - 1, [0 | List]).

build_matrix(Rows, Columns) ->
    build_matrix(Rows, Columns, []).

build_matrix(0, 0, Matrix) ->
    Matrix;
build_matrix(Rows, Columns, Matrix) ->
    append_list_to_matrix(Rows, build_list(Columns, Matrix), Matrix).
    
append_list_to_matrix(0, _List, Matrix) ->
    Matrix;
append_list_to_matrix(Rows, List, Matrix) ->
    append_list_to_matrix(Rows - 1, List, [List | Matrix]).
    
print() ->
    io:format("~p", [build_matrix(3,3)]).
%%% EUNIT Tests
default_has_all_zeros_test() ->
    ?assert([[0,0],[0,0]] =:= default(2,2)).
    
default_jagged_test() ->
    ?assert([[0,0,0],[0,0,0]] =:= default(2,3)).
    
default_needs_integers_test() ->
    ?assertError(function_clause, default('Foo', 2)),
    ?assertError(function_clause, default(2, 'Bar')).
    
build_list_test() ->
    ?assert([0,0,0] =:= build_list(3)).
    
build_matrix_test() ->
    ?assert([[0,0,0],[0,0,0],[0,0,0]] =:= build_matrix(3, 3)).
