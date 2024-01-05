-module(e2l).
-export([main/1]).

main([Filename]) ->
  { ok, <<"FOR1",Sz:32,Data:Sz/binary>> } = file:read_file(Filename),
  <<"BEAM",ChunksData/binary>> = Data,
  Chunks = parse_chunks(ChunksData),
  io:format("~p~n", [Chunks]).

parse_chunks(X) -> parse_chunks(X, #{}).
parse_chunks(<<>>, Acc) -> Acc;
parse_chunks(<<A:8, B:8, C:8, D:8, Sz:32, Blob/binary>>, Acc) ->
  Id = [A, B, C, D],
  PadSz = 4 * ((Sz + 3) div 4),
  <<Data:PadSz/binary, Rest/binary>> = Blob,
  parse_chunks(Rest, Acc#{Id => {Sz, Data}}).
