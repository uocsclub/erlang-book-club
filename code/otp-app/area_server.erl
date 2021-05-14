-module(area_server).
-behaviour(gen_server).

-export([area/1, start_link/0]).

%% here's all the gen_server stuff. You should remember this from last
%% time.
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Note 20k is a timeout
area(Thing) ->
    gen_server:call(?MODULE, {area, Thing}, 20000).

init([]) ->
    %% this ensures that terminate will be called when we halt
    process_flag(trap_exit, true),
    io:format("~p starting now~n", [?MODULE]),
    {ok, 0}.
    

handle_call({area, {square, S}}, _From, N) ->
    {reply, S * S, N + 1};
handle_call({area, {rectangle, B, H}}, _From, N) ->
    {reply, B * H, N + 1}.


handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.
terminate(_Reason, _N) -> io:format("~p stopping~n",[?MODULE]), ok.
code_change(_OldVsn, N, _Extra) -> {ok, N}.
