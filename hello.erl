-module(hello).
-export([main/1]).

main([]) -> main(["World"]);
main([Name]) -> io:format("hello, ~s~n", [Name]).
