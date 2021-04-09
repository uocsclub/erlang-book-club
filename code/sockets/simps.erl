-module(simps). % simple server
-compile(export_all).

% Listener logic starts here
start_server(Port) ->
    %% Note how I'm hardcoding {packet, 4} to make passing data easy
    %% for now between erlang systems.
    {ok, Listener} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
    spawn(fun() -> lhandler(Listener) end).


lhandler(Listener) ->
    {ok, Socket} = gen_tcp:accept(Listener),
    spawn(fun() -> lhandler(Listener) end),
    main(Socket).

% Listener logic ends here 

% Program logic starts here, only dealing with connections (Sockets)

% This part I just copied verbatim from the book, with some minor
% minor adjustments. It packs and unpacks erlang terms, which will let
% us talk to this server from another erlang node and send structured
% erlang data transparently. It's a pretty cool system, especially
% because it would be very easy to completely hide authentication as a
% layer around our actual message passing, but completely hidden from
% the "user" (the developer).
main(S) ->
    receive
        {tcp, S, Bin} ->
            %% io:format("Received binary = ~p~n", [Bin]),
            String = binary_to_term(Bin),
            io:format("received query: ~p~n", [String]),
            Response = eval(String),
            io:format("response: ~p~n", [Response]),
            gen_tcp:send(S, term_to_binary(Response)),
            main(S);
        {tcp_closed, S} ->
            io:format("socket closed~n")
    end.

eval(String) ->
    {ok, A, _} = erl_scan:string(String),
    {ok, B} = erl_parse:parse_exprs(A),
    {value, Thing, _} = erl_eval:exprs(B, []),
    Thing.
 
