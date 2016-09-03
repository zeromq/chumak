%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(rep_server).
-export([main/0]).

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(rep, "my-rep"),

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
    Reply = chumak:recv(Socket),
    io:format("Question: ~p\n", [Reply]),
    chumak:send(Socket, <<"Hello reply">>),
    loop(Socket).
