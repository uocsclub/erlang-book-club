-module(kvs).
-export([start/0, store/2, lookup/1, rpc/1]).

rpc(Query) -> 
    kvs_server ! {self(), Query},
    receive
        {kvs_server, Reply} ->
            Reply
    end.

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

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

start() -> register(kvs_server, spawn(fun() -> loop() end)).
