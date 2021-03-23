-module(torrent).
-compile(export_all).
% -export([]).

-record(torrent, {announce, files, name, piece_len, pieces, info_hash}).

%% This loads a torrent file and sets the record appropriately
load_torrent(FileRef) ->
    {<<>>, D} = benc:decode(FileRef),
    Info = get_assoc(<<"info">>, hd(D)),
    {D, {#torrent{
            announce = get_assoc(<<"announce">>, hd(D)),
            files = files(Info),
            name = get_assoc(<<"name">>, Info),
            piece_len = get_assoc(<<"piece length">>, Info),
            pieces = split_hashes(get_assoc(<<"pieces">>, Info)), 
            info_hash = crypto:hash(sha, benc:encode_item([{<<"info">>, Info}]))
           }}}.

%% This is to differentiate between when a torrent is a single file
%% and when it is multiple files
files(Info) ->
    case get_assoc(<<"length">>, Info) of
        undefined ->
            {files, get_assoc(<<"files">>, Info)};
        I ->
            {file, I}
    end.

%% Scans through a list of {K, V} tuples for Key and returns its Val
get_assoc(Key, L) ->
    case L of
        [{Key, V} | _Tail] ->
            V;
        [_H|T] ->
            get_assoc(Key, T);
        [] ->
            undefined
    end.

%% This is a utility function to split the piece hashes from a giant
%% blob into 20-byte-long hashes
split_hashes(Bin) when is_binary(Bin) ->
    split_hashes(Bin, []).
split_hashes(<<First:20/binary, Rest/binary>>, Acc) ->
    split_hashes(Rest, [First | Acc]);
split_hashes(<<>>, Acc) ->
    lists:reverse(Acc).
