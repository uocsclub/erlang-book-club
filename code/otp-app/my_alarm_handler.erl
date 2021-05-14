-module(my_alarm_handler).
-behaviour(gen_event).

-export([init/1, code_change/3, handle_event/2, 
         handle_call/2, handle_info/2, terminate/2]).

%% init args returns {ok, State}
init(Args) ->
    io:format("*** ~p init: ~p~n", [?MODULE, Args]),
    {ok, 0}.

%% N is the state
handle_event({set_alarm, tooHot}, N) ->
    logger:error("*** turn on the fan, it's too hot!"),
    {ok, N + 1};
handle_event({clear_alarm, tooHot}, N) ->
    logger:error("*** we can turn off the fan again"),
    {ok, N};
handle_event(Event, N) ->
    logger:error("*** unknown event ~p~n", [Event]),
    {ok, N}.

%% returns {ok, Reply, State}
handle_call(_Request, N) ->
    Reply = N,
    {ok, Reply, N}.
handle_info(_Info, N) ->
    {ok, N}.

terminate(_Reason, _N) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
