-module(benc).
-compile(export_all).
%% -export([decode/1, encode/1, encode_item/1]).

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

%% This is to compute integer powers because erlang can't do that lol
pow(_, 0) -> 1;
pow(A, 1) -> A;
pow(A, N) -> A * pow(A, N-1).

%% This section does encoding of the representation that I made for
%% bencoded data above.

%% WARNING: I think the bencode specification requires that keys are
%% in lexographic order. This does not check to make sure the keys are
%% in lexographic order for a dictionary.

%% fUnCtIoNaL pRoGraMmInG
%% encode_dict(L) -> 
%%     <<$d,
%%       (lists:foldl(
%%          fun(A, Bin) ->
%%                  <<Bin/binary, A/binary>>
%%          end,
%%          <<>>,
%%          lists:map(fun
%%                        ({K, V}) -> 
%%                            <<(encode_item(K))/binary, (encode_item(V))/binary>>
%%                    end, 
%%                    L)
%%         ))/binary,
%%       $e>>.

encode_dict(L) -> 
    <<$d,
      (list_to_binary(lists:map(fun
                                   ({K, V}) -> 
                                      <<(encode_item(K))/binary, (encode_item(V))/binary>>
                              end, 
                               L)))/binary,
      $e>>.

encode_bin(B) -> <<(integer_to_binary(byte_size(B)))/binary, $:, B/binary>>.

encode_list(L) -> 
    <<$l, 
      (list_to_binary(lists:map(fun(X) -> encode_item(X) end, L)))/binary, 
      $e>>.

encode_int(I) -> 
    <<$i, (integer_to_binary(I))/binary, $e>>.

encode_item(X) ->
    if
        is_list(X), length(X) > 0 andalso is_tuple(hd(X)), size(hd(X)) =:= 2 ->
            encode_dict(X); 
        is_list(X) ->
            encode_list(X);
        is_binary(X) ->
            encode_bin(X);
        is_integer(X) ->
            encode_int(X)
    end.

encode(L) ->
    encode(L, []).
encode([], Acc) ->
    list_to_binary(lists:reverse(Acc));
encode([H|T], Acc) ->
    encode(T, [encode_item(H) | Acc]).
