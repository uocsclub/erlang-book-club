-module(simple_server).
-export([start/2, rpc/2]).

start(Name, Mod) ->
    register(Name, 
             spawn(fun 
                       () -> loop(Name, Mod, Mod:init()) 
                   end)).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive {Name, Response} ->
            Response
    end.

loop(Name, Mod, State) ->
    receive 
        {From, Request} ->
            {Response, NewState} = Mod:handle(Request, State),
            From ! {Name, Response},
            loop(Name, Mod, NewState)
    end.
