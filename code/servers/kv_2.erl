-module(kv_2).
%% Note that these are the two functions that are called in
%% simple_server
-export([init/0, set/2, val/1, handle/2]).
-import(server_with_swapping, [rpc/2]).


%% Let's write a key value store!
set(K, V) -> rpc(kvstore, {set, K, V}).
val(K) -> rpc(kvstore, {val, K}).

init() -> dict:new().
handle({set, K, V}, Dict) -> {ok, dict:store(K, V, Dict)};
handle({val, K}, Dict) -> {dict:find(K, Dict), Dict}.
