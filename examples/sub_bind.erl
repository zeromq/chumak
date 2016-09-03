
%% This examples demonstrates running a sub socket with a bind.
%% Make sure to start the pub socket on the other end with connect.
-module(sub_bind).
-export([main/0]).

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(sub),
    Topic = <<" ">>,
    chumak:subscribe(Socket, Topic),
    case chumak:bind(Socket, tcp, "localhost", 5555) of
        {ok, _BindPid} ->
            io:format("Binding OK with Pid: ~p\n", [Socket]);
        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p \n", [X])
    end,
    loop(Socket).

loop(Socket) ->
    {ok, Data1} = chumak:recv(Socket),
    io:format("Received ~p\n", [Data1]),
    loop(Socket).