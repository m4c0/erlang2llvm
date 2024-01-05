-module(e2l).
-export([main/1]).

main([Filename]) ->
  { ok, <<"FOR1",Sz:32/big,Data:Sz/binary>> } = file:read_file(Filename),
  <<"BEAM",ChunksData/binary>> = Data,
  Chunks = parse_chunks(ChunksData),
  export(Chunks).

%% Exporter

export(Chunks) ->
  export_lits(Chunks),
  export_impt(Chunks).

export_lits(#{literals := Lits}) -> export_lits(0, Lits).
export_lits(_, []) -> io:format("~n");
export_lits(N, [X|Lits]) ->
  io:format("@.str.~b = private unnamed_addr constant [~b x i8] c\"~s\"~n",
            [N, length(X), X]),
  export_lits(N + 1, Lits).

export_impt(#{imports := T, atoms := Atoms}) -> export_impt(T, Atoms).
export_impt([], _) -> io:format("~n");
export_impt([{Mod, Fn, Art}|T], Atoms) ->
  AM = lists:nth(Mod, Atoms),
  AF = lists:nth(Fn, Atoms),
  io:format("declare ptr @erl~b~s~b~s~b(",
            [length(AM), AM, length(AF), AF, Art]),
  fmt_imp_args(Art),
  io:format(")~n"),
  export_impt(T, Atoms).

fmt_imp_args(0) -> ok;
fmt_imp_args(1) -> io:format("ptr nocapture");
fmt_imp_args(N) -> io:format("ptr nocapture, "), fmt_imp_args(N - 1).

%% Raw parser

parse_chunks(X) -> parse_chunks(X, #{}).
parse_chunks(<<>>, Acc) -> Acc;
parse_chunks(<<Id:4/binary, Sz:32/big, Blob/binary>>, Acc) ->
  PaddedSz = 4 * ((Sz + 3) div 4),
  PadSz = PaddedSz - Sz,
  <<Data:Sz/binary, _:PadSz/binary, Rest/binary>> = Blob,
  parse_chunks(Rest, parse_chunk(binary_to_list(Id), Data, Acc)).

parse_chunk("AtU8", <<Sz:32/big, Data/binary>>, Acc) -> Acc#{atoms => atoms(Sz, Data)};
parse_chunk("Attr", Data, Acc) -> Acc#{attr => binary_to_term(Data)};
parse_chunk("CInf", Data, Acc) -> Acc#{cinf => binary_to_term(Data)};
parse_chunk("Code", <<HdSz:32/big, Hd:HdSz/binary, Code/binary>>, Acc) ->
  <<_InstSet:32/big, _OpcodeMax:32/big, _LabelCount:32/big, _FnCount:32/big, _/binary>> = Hd,
  Acc#{code => code(Code, [])};
parse_chunk("Dbgi", Data, Acc) -> Acc#{dbgi => binary_to_term(Data)};
parse_chunk("ExpT", <<Sz:32/big, Data/binary>>, Acc) -> Acc#{exports => impt(Sz, Data)};
parse_chunk("ImpT", <<Sz:32/big, Data/binary>>, Acc) -> Acc#{imports => impt(Sz, Data)};
parse_chunk("LitT", <<_UnSz:32/big, Comp/binary>>, Acc) ->
  Acc#{literals => lit(zlib:uncompress(Comp))};
parse_chunk("Meta", Data, Acc) -> Acc#{meta => binary_to_term(Data)};
parse_chunk(Id, Data, Acc) -> Acc#{Id => Data}.

atoms(Sz, Data) -> atoms(Sz, Data, []).
atoms(0, <<>>, Acc) -> lists:reverse(Acc);
atoms(Sz, <<Len:8/big, Str:Len/binary, Rest/binary>>, Acc) ->
  atoms(Sz - 1, Rest, [binary_to_list(Str)|Acc]).

impt(Sz, Data) -> impt(Sz, Data, []).
impt(0, <<>>, Acc) -> lists:reverse(Acc);
impt(Sz, <<Mod:32/big, Fn:32/big, Art:32/big, Rest/binary>>, Acc) ->
  impt(Sz - 1, Rest, [{Mod, Fn, Art}|Acc]).

lit(<<Count:32/big, Data/binary>>) -> lit(Count, Data, []).
lit(0, <<>>, Acc) -> lists:reverse(Acc);
lit(N, <<Sz:32/big, Lit:Sz/binary, Rest/binary>>, Acc) -> lit(N - 1, Rest, [binary_to_term(Lit)|Acc]).

code(<<1:8/big, Data/binary>>, Acc) -> opcode(label, 1, Data, Acc);
code(<<2:8/big, Data/binary>>, Acc) -> opcode(func_info, 3, Data, Acc);
code(<<3:8/big, Data/binary>>, Acc) -> opcode(int_code_end, 0, Data, Acc);
code(<<64:8/big, Data/binary>>, Acc) -> opcode(move, 2, Data, Acc);
code(<<78:8/big, Data/binary>>, Acc) -> opcode(call_ext_only, 2, Data, Acc);
code(<<153:8/big, Data/binary>>, Acc) -> opcode(line, 1, Data, Acc);
code(<<>>, Acc) -> lists:reverse(Acc);
code(Rem, Acc) -> lists:reverse([{failed, Rem}|Acc]).

opcode(Lbl, Arity, Data, Acc) ->
  {Args, Rest} = args(Arity, Data),
  code(Rest, [{Lbl, Args}|Acc]).

args(Arity, Data) -> args(Arity, [], Data).
args(0, Acc, Rest) -> {lists:reverse(Acc), Rest};
args(N, Acc, <<L:4/big, 0:1/big, 0:3/big, Rest/binary>>) -> args(N - 1, [{literal, L}|Acc], Rest);
args(N, Acc, <<L:4/big, 0:1/big, 1:3/big, Rest/binary>>) -> args(N - 1, [{int, L}|Acc], Rest);
args(N, Acc, <<L:4/big, 0:1/big, 2:3/big, Rest/binary>>) -> args(N - 1, [{atom, L}|Acc], Rest);
args(N, Acc, <<L:4/big, 0:1/big, 3:3/big, Rest/binary>>) -> args(N - 1, [{x, L}|Acc], Rest);
args(N, Acc, <<L:4/big, 0:1/big, 4:3/big, Rest/binary>>) -> args(N - 1, [{y, L}|Acc], Rest);
args(N, Acc, <<L:4/big, 0:1/big, 5:3/big, Rest/binary>>) -> args(N - 1, [{label, L}|Acc], Rest);
args(N, Acc, <<L:4/big, 0:1/big, 6:3/big, Rest/binary>>) -> args(N - 1, [{char, L}|Acc], Rest);
%% OTP-20 values only
args(N, Acc, <<4:4/big, 0:1/big, 7:3/big, Sz:8/big, Lit:Sz/big, Rest/binary>>) -> 
  args(N - 1, [{literal_ex, Lit}|Acc], Rest).
