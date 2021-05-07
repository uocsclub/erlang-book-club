-module(kv_3).
%% Note that these are the two functions that are called in
%% simple_server
-export([init/0, set/2, val/1, dump/0, handle/2]).
-import(server_with_swapping, [rpc/2]).

%% change here!
dump() -> rpc(kvstore, {dump}).
    

%% Let's write a key value store!
set(K, V) -> rpc(kvstore, {set, K, V}).
val(K) -> rpc(kvstore, {val, K}).

init() -> dict:new().
handle({set, K, V}, Dict) -> {ok, dict:store(K, V, Dict)};
handle({val, K}, Dict) -> {dict:find(K, Dict), Dict};
handle({dump}, Dict) -> {dict:to_list(Dict), Dict}.
     
