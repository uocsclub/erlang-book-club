-module(prime_server).
-behaviour(gen_server).

-export([new_prime/1, start_link/0]).

%% here's all the gen_server stuff. You should remember this from last
%% time.
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Note 20k is a timeout
new_prime(N) ->
    gen_server:call(?MODULE, {prime, N}, 20000).

init([]) ->
    %% this ensures that terminate will be called when we halt
    process_flag(trap_exit, true),
    io:format("~p starting now~n", [?MODULE]),
    {ok, 0}.
    

handle_call({prime, K}, _From, N) ->
    {reply, make_new_prime(K), N + 1}.


handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.
terminate(_Reason, _N) -> io:format("~p stopping~n",[?MODULE]), ok.
code_change(_OldVsn, N, _Extra) -> {ok, N}.

make_new_prime(K) ->
    if
        K > 100 ->
            alarm_handler:set_alarm(tooHot),
            %% Note this is because I don't have the jaerlang stuff
            %% set up so we're missing lib_primes
            N = {this_also_isnt_a_prime_but_its_hot, 1234123},
            alarm_handler:clear_alarm(tooHot),
            N;
        true ->
            %% Note this is because I don't have the jaerlang stuff
            %% set up so we're missing lib_primes
            {this_isnt_a_prime_lmao, 123411234}
    end.
