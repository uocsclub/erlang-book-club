-module(event_handler).
%% -compile(export_all).
-export([make/1, event/2, add_handler/2]).

%% spawn a new process with a no_op handler
make(N) -> register(N, spawn(fun() -> handler(fun no_op/1) end)).

%% swaps out the current event handling function (from no_op to something else) 
add_handler(N, F) -> N ! {add, F}.

event(N, X) -> N ! {event, X}.

handler(F) ->
    receive
        {add, F1} ->
            handler(F1);
        {event, E} ->
            (catch F(E)),
            handler(F)
    end.

no_op(_) -> void.
