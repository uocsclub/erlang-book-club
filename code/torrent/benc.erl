-module(benc).
%% -compile(export_all).
-export([decode/1]).


decode(Bin) when is_binary(Bin) ->
    parse_loop(Bin);
decode(X) when is_list(X) -> 
    %% this assumes that we're being passed a file name, I'm not sure
    %% what's the better style to take args like this 
    {ok, FileBin} = file:read_file(X),
    parse_loop(FileBin).


parse_loop(Bin) ->
    parse_loop(Bin, []).

%% This is the main loop. It takes a binary string and produces a list
%% of outputs. If it matches on d/l/i then it parses a
%% dict/list/integer, and then appends this to the list (Acc).

%% otherwise it reads the length of a binary string and then pushes
%% that binary onto Acc and recurses.

%% The whole thing is tail-recursive, so this should actually be
%% really fast (although it still uses GCd lists, so it's not _fast_
%% fast).
parse_loop(<<$d, Rest/binary>>, Acc) ->
    {R2, A2} = parse_loop(Rest, []),
    parse_loop(R2, [build_assoc(A2)|Acc]);
parse_loop(<<$i, Rest/binary>>, Acc) ->
    {Int, R} = parse_aint(Rest, $e),
    parse_loop(R, [Int|Acc]);
parse_loop(<<$l, Rest/binary>>, Acc) ->
    {R2, A2} = parse_loop(Rest, []),
    parse_loop(R2, [A2 | Acc]);
parse_loop(<<I:8, _Rest/binary>> = Bin, Acc) when I >= $1, I =< $9 ->
    {Int, R} = parse_aint(Bin, $:),
    parse_loop(binary:part(R, {Int, byte_size(R) - Int}), 
               [binary:part(R, {0, Int}) | Acc]);

parse_loop(<<$e:8, Rest/binary>>, Acc) -> {Rest, lists:reverse(Acc)};
parse_loop(<<$e:8>>, Acc) -> {<<>>, lists:reverse(Acc)};
parse_loop(<<>>, Acc) -> {<<>>, lists:reverse(Acc)}.

build_assoc([K, V|T]) ->
    [{K, V} | build_assoc(T)];
build_assoc([]) -> [].

parse_aint(Bin, End) ->
    parse_aint(Bin, End, []).
parse_aint(<<End, Rest/binary>>, End, Acc) -> % {int, rest_of_binary}
    {le_altoi(Acc), Rest};
parse_aint(<<I:8, Rest/binary>>, End, Acc) when I >= $0, I =< $9 -> 
    parse_aint(Rest, End, [I|Acc]).

le_altoi(L) -> % little-endian ascii-list-to-integer
    le_altoi(L, 0, 0).
le_altoi([H|T], Acc, Exp) ->
    le_altoi(T,
             (Acc + ((H - $0) * pow(10, Exp))), 
             Exp + 1);
le_altoi([], Acc, _exp) ->
    Acc.

% This is to compute integer powers because erlang can't do that lol
pow(_, 0) -> 1;
pow(A, 1) -> A;
pow(A, N) -> A * pow(A, N-1).


    
    
