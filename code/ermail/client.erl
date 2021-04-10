-module(client).
-compile(export_all).
-include("./mail.hrl").

reguser(User, Pass, Server) when is_list(User) ->
    reguser(list_to_binary(User), Pass, Server);
reguser(User, Pass, Server) when is_list(Pass) ->
    reguser(User, list_to_binary(Pass), Server);
reguser(User, Pass, Server) ->
    {ok, S} = gen_tcp:connect(Server, ?PORT, [binary, {packet, 4}, {active, true}]),
    gen_tcp:send(S, <<$r, (benc:encode_list([User, Pass]))/binary>>),
    receive
        {tcp, S, <<"success">>} ->
            true;
        {tcp, S, Bin} ->
            {error, Bin};
        {tcp_closed, S} ->
            {error, closed}
    after 
        1000 ->
            {error, timeout}
    end.

send(To, From, Cc, Body, Server) ->
    {ok, S} = gen_tcp:connect(Server, ?PORT, [binary, {packet, 4}, {active, true}]),
    gen_tcp:send(S, <<$s, (benc:encode_list([To, From, Cc, Body]))/binary>>),
    receive
        {tcp, S, <<"success">>} ->
            true;
        {tcp, S, Bin} ->
            {error, Bin};
        {tcp_closed, S} ->
            {error, closed}
    after 1000 -> {error, timeout}
    end.

getmail(User, Pass, Server) ->
    getmail(User, Pass, 0, Server).
getmail(User, Pass, NGotten, Server) when is_list(User) ->
    getmail(list_to_binary(User), Pass, NGotten, Server);
getmail(User, Pass, NGotten, Server) when is_list(Pass) ->
    getmail(User, list_to_binary(Pass), NGotten, Server);

getmail(User, Pass, NGotten, Server) ->
    {ok, S} = gen_tcp:connect(Server, ?PORT, [binary, {packet, 4}, {active, true}]),
    gen_tcp:send(S, <<$g, (benc:encode_list([User, Pass, NGotten]))/binary>>),
    receive
        {tcp, S, <<"success">>} ->
            true;
        {tcp, S, Bin} ->
            {mail, benc:decode(Bin)};
        {tcp_closed, S} ->
            {error, closed}
    after 1000 -> {error, timeout}
    end.
