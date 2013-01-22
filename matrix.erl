-module(matrix).

-export([default/2]).

-include_lib("eunit/include/eunit.hrl").

default(Rows, Columns) when is_integer(Rows), is_integer(Columns) ->
    %build_matrix(0, Rows, Columns, []).
    [[0,0],[0,0]].

build_list(Size) ->
    build_list(Size, []).

build_list(0, List) ->
    List;
build_list(Size, List) ->
    build_list(Size - 1, [0 | List]).

%%% EUNIT Tests
default_has_all_zeros_test() ->
    ?assert([[0,0],[0,0]] =:= default(2,2)).
    
default_needs_integers_test() ->
    ?assertError(function_clause, default('Foo', 2)),
    ?assertError(function_clause, default(2, 'Bar')).
build_list_test() ->
    ?assert([0,0,0] =:= build_list(3)).
%default_jagged_test() ->
    %?assert([[0,0],[0,0,0]] =:= default(2,3)).

%        public Matrix(int rows, int columns)
%        {
%            _matrix = new double[rows, columns];
%            _rows = rows;
%            _columns = columns;
%            for (var i = 0; i < rows; i++)          For every row
%                for (var j = 0; j < columns; j++)   For every column
%                    _matrix[i, j] = 0;              Initialize the row, column intersection with zero
%        }