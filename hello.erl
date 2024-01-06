-module(hello).
-export([main/1]).

main([]) -> main(["World"]);
main([Name]) -> print(Name).

print(Name) -> io:format("hello, ~s~n", Name).
