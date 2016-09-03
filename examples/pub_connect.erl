%% This examples demonstrates using a pub socket with connect.
%% Make sure to start the sub socket on the other end with a bind.
-module(pub_connect).
-export([main/0]).

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(pub),

    case chumak:connect(Socket, tcp, "localhost", 5555) of
        {ok, _BindPid} ->
            io:format("Binding OK with Pid: ~p\n", [Socket]);
        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p \n", [X])
    end,
    loop(Socket).

loop(Socket) ->
    ok = chumak:send(Socket, <<" ", "Hello world">>),
    timer:sleep(1000),
    loop(Socket).