-module(seans).
-export([integerify/1, store/2, lookup/1, start_seans/0]).

rpc(Query) -> 
    seans ! {self(), Query},
    receive
        {kvs_server, Reply} ->
            Reply
    end.

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

start_seans() -> 
    Pids = [spawn(kvs, loop, []) || _X <- lists:seq(1, 10)],
    register(seans, spawn(fun() -> loop(Pids) end)).

integerify(X) when is_tuple(X) ->
    integerify(tuple_to_list(X));
integerify(X) when is_list(X) ->
    lists:sum(lists:map(fun (Y) -> integerify(Y) end, X));
integerify(X) when is_atom(X) ->
    lists:sum(atom_to_list(X));
integerify(X) when is_integer(X) -> X.

loop(Pids) ->
    receive
        {From, {store, Key, Value}} ->
            Child = lists:nth((integerify(Key) rem 10) + 1, Pids),
            Child ! {From, {store, Key, Value}},
            loop(Pids);
        {From, {lookup, Key}} ->
            Child = lists:nth((integerify(Key) rem 10) + 1, Pids),
            Child ! {From, {lookup, Key}},
            loop(Pids)
    end.

    
