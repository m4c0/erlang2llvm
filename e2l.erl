-module(e2l).
-export([parse/1]).

parse(Filename) ->
  { ok, <<"FOR1",Sz:32/big,Data:Sz/binary>> } = file:read_file(Filename),
  <<"BEAM",ChunksData/binary>> = Data,
  Chunks = parse_chunks(ChunksData),
  %% io:format("~p~n", [Chunks]),
  export(Chunks).

%% Exporter

export(Chunks) ->
  export_lits(Chunks),
  export_impt(Chunks),
  export_implicits(),
  export_code(Chunks).

export_implicits() ->
  io:format("declare void @e2l_error() noreturn~n").

%% Using the process storage for anything "global" when exporting the code.
%% This makes the whole exporter simpler. Erlang is based on Prolog, not
%% Haskell... :)

export_code(#{code := Code, atoms := Atoms, exports := ExpT}) ->
  put(atoms, Atoms),
  put(exports, ExpT),
  lists:foreach(fun export_opcode/1, Code).

export_opcode({func_info, [{atom, M}, {atom, F}, {literal, A}]}) ->
  close_fn(),
  io:format("; ~s~n", [erl_fn_name(M, F, A)]),
  put(func_info, {M, F, A});
export_opcode({int_code_end, []}) -> close_fn();
export_opcode({label, [{literal, L}]}) ->
  case get(func_info) of
    {M, F, A} ->
      Link = linkage(F, A, get(exports)),
      Name = pub_fn_name(M, F, A, get(atoms)),
      io:format("define ~sptr ~s(", [Link, Name]),
      fmt_fn_args(A),
      io:format(") unnamed_addr {~n"),
      put(func_info, undefined),
      %% uses the label before "func_info" as the exception handler
      put(open_fn, get(pend_lbl));
    undefined -> undefined
  end,
  put(pend_lbl, L);
export_opcode({move, [{x, Src}, {x, Dst}]}) ->
  io:format("  %x~b = load ptr, ptr %x~b~n", [Dst, Src]);
export_opcode({line, _}) -> undefined;
export_opcode(C) ->
  emit_pend_label(),
  io:format("; unsupported: ~p~n", [C]).

linkage(_, _, []) -> "private ";
linkage(F, A, [{F, A, _}|_]) -> "";
linkage(F, A, [_|E]) -> linkage(F, A, E).

emit_pend_label() ->
  case get(pend_lbl) of
    undefined -> undefined;
    N ->
      io:format("lbl~b:~n", [N]),
      put(pend_lbl, undefined)
  end.

close_fn() ->
  case get(open_fn) of
    undefined -> undefined;
    N ->
      io:format("lbl~b:~n", [N]),
      io:format("  call void e2l_error~n"),
      io:format("  unreachable~n"),
      io:format("}~n~n"),
      put(open_fn, undefined)
  end.

export_lits(#{literals := Lits}) -> export_lits(0, Lits).
export_lits(_, []) -> io:nl();
export_lits(N, [X|Lits]) when is_map(X) ->
  io:format("; map literals (id = ~b) is not supported~n", [N]),
  export_lits(N + 1, Lits);
export_lits(N, [X|Lits]) when is_list(X) ->
  io:format("@.str.~b = private unnamed_addr constant [~b x i8] c\"~s\"~n",
            [N, length(X), X]),
  export_lits(N + 1, Lits).

export_impt(#{imports := T, atoms := Atoms}) -> export_impt(T, Atoms).
export_impt([], _) -> io:nl();
export_impt([{Mod, Fn, Art}|T], Atoms) ->
  PubName = pub_fn_name(Mod, Fn, Art, Atoms),
  io:format("declare ptr ~s(", [PubName]),
  fmt_imp_args(Art),
  io:format(")~n"),
  export_impt(T, Atoms).

fmt_fn_args(0) -> ok;
fmt_fn_args(1) -> io:format("ptr nocapture");
fmt_fn_args(N) -> io:format("ptr nocapture, "), fmt_fn_args(N - 1).

fmt_imp_args(0) -> ok;
fmt_imp_args(1) -> io:format("ptr nocapture");
fmt_imp_args(N) -> io:format("ptr nocapture, "), fmt_imp_args(N - 1).

pub_fn_name(Mod, Fn, Art, Atoms) ->
  AM = lists:nth(Mod, Atoms),
  AF = lists:nth(Fn, Atoms),
  io_lib:format("@erl~b~s~b~s~b", [length(AM), AM, length(AF), AF, Art]).

erl_fn_name(Mod, Fn, Art) -> 
  Atoms = get(atoms),
  Args = [lists:nth(Mod, Atoms), lists:nth(Fn, Atoms), Art],
  io_lib:format("~s:~s/~b", Args).

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

code(<<N:8/big, Data/binary>> = Rem, Acc) ->
  Opcodes = #{1 => {label, 1},
              2 => {func_info, 3},
              3 => {int_code_end, 0},
              6 => {call_only, 2},
              7 => {call_ext, 2},
              8 => {call_ext_last, 3},
              12 => {allocate, 2},
              19 => {return, 0},
              52 => {is_nil, 2},
              56 => {is_nonempty_list, 2},
              64 => {move, 2},
              65 => {get_list, 3},
              66 => {get_tuple_element, 3},
              78 => {call_ext_only, 2},
              153 => {line, 1},
              159 => {is_tagged_tuple, 4},
              163 => {get_tl, 2},
              166 => {bs_start_match3, 4},
              182 => {bs_match, 3}},
  case Opcodes of
    #{N := {Opcode, Arity}} -> opcode(Opcode, Arity, Data, Acc);
    _ -> lists:reverse([{failed, Rem}|Acc])
  end;
code(<<>>, Acc) -> lists:reverse(Acc).

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
args(N, Acc, <<1:4/big, 0:1/big, 7:3/big, Size:8/big, Data/binary>>) -> 
  {Args, Rest} = args(Size, Data),
  args(N - 1, [{list_ex, Args}|Acc], Rest);
args(N, Acc, <<4:4/big, 0:1/big, 7:3/big, Lit:8/big, Rest/binary>>) -> 
  args(N - 1, [{literal_ex, Lit}|Acc], Rest).
