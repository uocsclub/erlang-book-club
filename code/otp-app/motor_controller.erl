-module(motor_controller).
-export([add_event_handler/0]).

add_event_handler() ->
    event_handler:add_handler(errors, fun controller/1).

controller(too_hot) ->
    io:format("Turning off the motor~n");
controller(AnythingElse) ->
    io:format("~w Ignoring non-too_hot event ~p~n", [?MODULE, AnythingElse]).
