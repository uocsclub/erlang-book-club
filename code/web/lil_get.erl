-module(lil_get).

%% Note this flag should never be used with libraries that are ever in
%% communication with anyone else, but it is supremely useful for
%% iteration and quick debugging.
-compile(export_all).

lil_get(Host) ->
    %% creates a socket to Host on port 80 of type binary without
    %% erlang's packet headers (size 0)
    {ok, Sock} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    %% this pattern match just crashes on error, which is by design.
    ok = gen_tcp:send(Sock, "GET / HTTP/1.0\r\n\r\n"),
    recv_http(Sock, []).

recv_http(Sock, Acc) ->
    receive
        {tcp, Sock, Bin} ->
            recv_http(Sock, [Bin|Acc]);
        {tcp_closed, Sock} ->
            list_to_binary(lists:reverse(Acc))
    end.
