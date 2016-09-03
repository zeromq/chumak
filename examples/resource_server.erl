%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(resource_server).
-export([main/0]).

main() ->
    application:start(chumak),
    {ok, Resource} = chumak:resource(),
    {ok, SocketA} = chumak:socket(rep, "A"),
    {ok, SocketB} = chumak:socket(rep, "B"),

    chumak:attach_resource(Resource, "service/a", SocketA),
    chumak:attach_resource(Resource, "service/b", SocketB),

    spawn_link(fun () ->
                       loop(SocketA, <<"Hello A">>)
               end),
    spawn_link(fun () ->
                       loop(SocketB, <<"Hello B">>)
               end),
    {ok, _BindPid} = chumak:bind(Resource, tcp, "localhost", 5555),

    receive
        _ -> ok
    end.

loop(Socket, Msg) ->
    Data = chumak:recv(Socket),
    chumak:send(Socket, <<"Reply from: ", Msg/binary, " is ", Msg/binary>>),
    loop(Socket, Msg).
