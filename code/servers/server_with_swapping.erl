-module(server_with_swapping).
-export([start/2, rpc/2, swap_code/2]).

start(Name, Mod) ->
    register(Name,
             spawn(fun() -> loop(Name,Mod,Mod:init()) end)).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, Response} -> Response
    end.

loop(Name, Mod, State) ->
    receive
        {From, {swap_code, NewCallBackMod}} ->
            From ! {Name, ack},
            loop(Name, NewCallBackMod, State);
        {From, Request} ->
            try Mod:handle(Request, State) of
                {Response, NewState} ->
                    From ! {Name, Response},
                    loop(Name, Mod, NewState)
            catch
                %% we don't actually care what this is
                _:_Why ->
                    From ! {Name, crash},
                    loop(Name, Mod, State)
            end
    end.
