-module(e2l).
-export([main/1]).

main([Filename]) ->
  { ok, <<"FOR1",Sz:32/big,Data:Sz/binary>> } = file:read_file(Filename),
  <<"BEAM",ChunksData/binary>> = Data,
  Chunks = parse_chunks(ChunksData),
  io:format("~p~n", [Chunks]).

parse_chunks(X) -> parse_chunks(X, #{}).
parse_chunks(<<>>, Acc) -> Acc;
parse_chunks(<<Id:4/binary, Sz:32/big, Blob/binary>>, Acc) ->
  PaddedSz = 4 * ((Sz + 3) div 4),
  PadSz = PaddedSz - Sz,
  <<Data:Sz/binary, _:PadSz/binary, Rest/binary>> = Blob,
  parse_chunks(Rest, parse_chunk(binary_to_list(Id), Data, Acc)).

parse_chunk("Code", <<HdSz:32/big, Hd:HdSz/binary, Code/binary>>, Acc) ->
  <<_InstSet:32/big, _OpcodeMax:32/big, _LabelCount:32/big, _FnCount:32/big, _/binary>> = Hd,
  Acc#{code => code(Code, [])};
parse_chunk(_, _, Acc) -> Acc.

code(<<1:8/big, N:5/big, 0:3/big, Rest/binary>>, Acc) -> code(Rest, [{label, N}|Acc]);
code(<<2:8/big, Rest/binary>>, Acc) -> code(Rest, [{func_info}|Acc]);
code(<<153:8/big, N:5/big, 0:3/big, Rest/binary>>, Acc) -> code(Rest, [{line, N}|Acc]);
code(<<>>, Acc) -> lists:reverse(Acc).
