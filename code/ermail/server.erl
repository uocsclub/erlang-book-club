-module(server).
-compile(export_all).
-include("./mail.hrl").
-include_lib("kernel/include/file.hrl").

init_maildir(Dir) ->
    {ok, Info} = file:read_file_info(Dir),
    directory = Info#file_info.type,
    ok = file:make_dir(filename:join([Dir, "mailboxes"])),
    ok = file:make_dir(filename:join([Dir, "messages"])),
    ok = file:write_file(filename:join([Dir, "users"]), <<>>).
    

start_server() ->
    start_server("./mail").

start_server(MailDir) ->
    {ok, LS} = gen_tcp:listen(?PORT, [binary, {packet, 4}, {reuseaddr, true}, {active, false}]),
    true = register(server, spawn(fun() -> lhandler(LS, filename:join([MailDir])) end)).

lhandler(LS, MailDir) ->
    {ok, S} = gen_tcp:accept(LS),
    Pid = spawn(fun() -> chandler(S, MailDir) end),
    gen_tcp:controlling_process(S, Pid),
    Pid ! go,
    lhandler(LS, MailDir).
    
chandler(S, MailDir) ->
    receive
        go ->
            true
    end,
    inet:setopts(S, [{active, true}]),
    cloop(S, MailDir).

cloop(S, MailDir) ->
    receive
        {tcp, S, Bin} ->
            io:format("Server received command: ~p~n", [Bin]),
            Val = dispatch_msg(Bin, MailDir, S),
            io:format("server got: ~p~n", [Val]),
            gen_tcp:send(S, <<"success">>),
            cloop(S, MailDir);
        {tcp_closed, S} ->
            io:format("Socket closed")
    end.
 
dispatch_msg(<<$r, Rest/binary>>, MailDir, S) ->
    {<<>>, [[User, Pass]]} = benc:decode(Rest),
    apply_reg(User, Pass, MailDir, S);
dispatch_msg(<<$s, Rest/binary>>, MailDir, S) ->
    {<<>>, [[To, From, Cc, Body]]} = benc:decode(Rest),
    apply_send(To, From, Cc, Body, MailDir, S);
dispatch_msg(<<$g, Rest/binary>>, MailDir, S) ->
    {<<>>, [[User, Pass, Number]]} = benc:decode(Rest),
    apply_get(User, Pass, Number, MailDir, S);
dispatch_msg(_, _, _) ->
    false.

apply_reg(User, Pass, MailDir, _S) ->
    %% If user contains the allowed characters (there was probably
    %% a better way to write this, but that's fine.
    UserGood = (User =:= 
                    list_to_binary(
                      lists:filter(fun 
                                       (X) -> 
                                          (X >= $a andalso X =< $z) 
                                              orelse (X >= $A andalso X =< $Z)
                                              orelse (X >= $0 andalso X =< $9)
                                              orelse (X =:= $_) orelse (X =:= $-)
                                   end, binary_to_list(User)))),
    if 
        UserGood ->
            {ok, File} = file:open(filename:join([MailDir, "users"]), [write, append]),
            ok = file:write_file(
                   filename:join([MailDir, "mailboxes", binary_to_list(User)]), 
                   <<>>),
            file:write(File, list_to_binary([benc:encode_item(User), benc:encode_item(Pass)])),
            true;
        true ->
            {error, baduser}
    end.

fixl(A) ->
    lists:map(fun
                  (X) when is_binary(X) ->
                      X;
                  (X) when is_list(X) ->
                      list_to_binary(X)
              end, A).

get_unique_mail_filename() ->
    lists:concat(["mail-", integer_to_list(erlang:unique_integer())]).
apply_send(To, From, Cc, Body, MailDir, S) when is_list(To), is_integer(hd(To)) ->
    apply_send([list_to_binary(To)], From, Cc, Body, MailDir, S);
apply_send(To, From, Cc, Body, MailDir, S) when is_list(From), is_integer(hd(From)) ->
    apply_send(To, list_to_binary(From), Cc, Body, MailDir, S);
apply_send(To, From, Cc, Body, MailDir, S) when is_list(Cc), is_integer(hd(Cc)) ->
    apply_send(To, From, [list_to_binary(Cc)], Body, MailDir, S);
apply_send(To, From, Cc, Body, MailDir, S) when is_list(Body) ->
    apply_send(To, From, Cc, list_to_binary(Body), MailDir, S);
apply_send(To, From, Cc, Body, MailDir, S) ->
    apply_sendl(fixl(To), From, fixl(Cc), Body, MailDir, S).
apply_sendl(To, From, Cc, Body, MailDir, _S) ->
    MailFile = get_unique_mail_filename(),
    ok = file:write_file(filename:join([MailDir, "messages", MailFile]), 
                         benc:encode_item([To, From, Cc, Body])),
    AlertFun = fun(X) ->
                       case file:open(filename:join([MailDir, "mailboxes", X]), [write, append]) of
                           {ok, File} ->
                               ok = file:write(File, [MailFile, $\n]);
                           {error, Reason} ->
                               {false, Reason}
                       end
               end,
    lists:map(AlertFun, To),
    lists:map(AlertFun, Cc),
    true.

find(Pred, [H|T]) ->
    Val = Pred(H),
    if Val =:= true ->
            H;
       true ->
            find(Pred, T)
    end.

check(User, Pass, MailDir) ->
    {ok, UsersFile} = file:read_file(filename:join([MailDir, "users"])),
    Thing = lists:map(
              fun(X) -> {<<>>, S} = benc:decode(X), S end,
              binary:split(UsersFile, <<$\n>>)),
    [] =/= find(fun(X) -> [User, Pass] =:= X end, Thing).

apply_get(User, Pass, Number, MailDir, S) ->
    true = check(User, Pass, MailDir),
    {ok, MailBox} = file:read_file(filename:join([MailDir, "mailboxes", User])),
    Messages = lists:nthtail(Number, binary:split(MailBox, <<$\n>>, [trim_all, global])),
    erlang:display(Messages),
    ok = gen_tcp:send(
           S, 
           lists:map(fun(X) ->
                             {ok, F} = file:read_file(filename:join([MailDir, "messages", X])),
                             F
                     end, Messages)),
    true.
