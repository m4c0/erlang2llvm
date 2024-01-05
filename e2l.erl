-module(e2l).
-export([main/1]).

main([Filename]) ->
  { ok, <<"FOR1",Sz:32,Data:Sz/binary>> } = file:read_file(Filename),
  <<"BEAM",ChunksData/binary>> = Data,
  Chunks = parse_chunks(ChunksData),
  io:format("~p~n", [Chunks]).

parse_chunks(X) -> parse_chunks(X, #{}).

parse_chunks(<<>>, Acc) -> Acc;
parse_chunks(<<Id:4/binary, Sz:32, Blob/binary>>, Acc) ->
  PadSz = 4 * ((Sz + 3) div 4),
  <<Data:PadSz/binary, Rest/binary>> = Blob,
  parse_chunks(Rest, Acc#{binary_to_list(Id) => {Sz, Data}}).

