%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(resource_client).
-export([main/0]).

main() ->
    application:start(chumak),

    spawn_link(fun () ->
                       service("service/a")
               end),
    spawn_link(fun () ->
                       service("service/b")
               end),
    spawn_link(fun () ->
                       service("service/c")
               end),

    receive
        _ -> ok
    end.

service(Resource) ->
    {ok, Socket} = chumak:socket(req),
    case chumak:connect(Socket, tcp, "localhost", 5555, Resource) of
        {ok, Pid} ->
            send_messages(Socket, []);

        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        Reply ->
            io:format("Unhandled reply for connect ~p \n", [Reply])
    end.

send_messages(Socket, []) ->
    send_messages(Socket, [
                           <<"Hello my dear friend">>,
                           <<"Hello my old friend">>,
                           <<"Hello all the things">>
                          ]);

send_messages(Socket, [Message|Messages]) ->
    case chumak:send(Socket, Message) of
        ok ->
            io:format("Send message: ~p\n", [Message]);
        {error, Reason} ->
            io:format("Failed to send message: ~p, reason: ~p\n", [Message, Reason])
    end,
    case chumak:recv(Socket) of
        {ok, RecvMessage} ->
            io:format("Recv message: ~p\n", [RecvMessage]);
        {error, RecvReason} ->
            io:format("Failed to recv, reason: ~p\n", [RecvReason])
    end,
    timer:sleep(1000),
    send_messages(Socket, Messages).
