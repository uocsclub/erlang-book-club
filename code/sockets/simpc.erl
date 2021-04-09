-module(simpc). % simple client
-compile(export_all).

do_rpc(Port, ErlStr) when is_integer(Port), is_list(ErlStr) ->
    do_rpc("localhost", Port, ErlStr).
do_rpc(Addr, Port, ErlStr) ->
    %% Same {packet, 4} hardcode.
    {ok, S} = gen_tcp:connect(Addr, Port, [binary, {packet, 4}]),
    ok = gen_tcp:send(S, term_to_binary(ErlStr)),
    receive
        {tcp, S, Bin} ->
            Res = binary_to_term(Bin),
            io:format("server response: ~p~n", [Res]),
            gen_tcp:close(S)
    after 2000 ->
            gen_tcp:close(S)
    end.
