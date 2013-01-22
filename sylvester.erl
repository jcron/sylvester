-module(sylvester).

-export([hello_world/0]).

-include_lib("eunit/include/eunit.hrl").

hello_world() ->
    "hello world!".

%%% EUNIT Tests
hello_world_test() ->
    ?assert("hello world!" =:= hello_world()).
