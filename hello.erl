-module(hello).
-export([main/1]).

main([Name]) -> io:format("hello, ~s~n", Name).
