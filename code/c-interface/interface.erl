-module(interface).
-compile(export_all).
%% -export([start/0, stop/0, twice/1, sum/2, log_and_le_encode/2, encode/1]).


start () ->
    register(interface,
             spawn(fun() ->
                           process_flag(trap_exit, true),
                           Port = open_port({spawn, "./interface"}, [{packet, 2}]),
                           loop(Port)
                   end)).

stop() -> ?MODULE ! stop.
twice(X) -> call_port({twice, X}).
sum(X, Y) -> call_port({sum, X, Y}).
call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
        {?MODULE, Result} ->
            Result
    end.
    
loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {?MODULE, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason})
    end.

% Integer log to know length
log_and_le_encode(N, Base) ->
    if 
        (N < Base) ->
            {0, [N]};
        (N >= Base) ->
            {LN, Repr} = log_and_le_encode(N div Base, Base),
            {LN + 1, [N rem Base | Repr]}
    end.
    

encode({sum, X, Y}) -> 
    {LX, RX} = log_and_le_encode(X, 256), % L -> Log, R -> Repr
    {LY, RY} = log_and_le_encode(Y, 256),
    [1, LX + 1] ++ RX ++ [LY + 1] ++ RY;
encode({twice, X}) -> 
    {LX, RX} = log_and_le_encode(X, 256),
    [2, LX + 1] ++ RX.

decode([_Size|LR]) -> 
    lists:foldr(fun 
                    (Elem, AccIn) -> 
                        AccIn * 256 + Elem 
                end, 0, LR). 
