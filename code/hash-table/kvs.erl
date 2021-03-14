-module(kvs).
-export([debug/0, loop/0]).

loop() ->
    receive
        {From, {store, Key, Value}} ->
            put(Key, {ok, Value}),
            From ! {kvs_server, true},
            loop();
        {From, {lookup, Key}} ->
            From ! {kvs_server, get(Key)},
            loop()
    end.

debug() ->
    receive 
        Msg ->
            erlang:display("Y {"), erlang:display(Msg),
            erlang:display("} Y")
    end, 
    debug().
