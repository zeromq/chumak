%% @copyright 2016 Choven Corp.
%%
%% This file is part of erlangzmq.
%%
%% erlangzmq is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% erlangzmq is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with erlangzmq.  If not, see <http://www.gnu.org/licenses/>
-module(resource_client).
-export([main/0]).

main() ->
    application:start(erlangzmq),

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
    {ok, Socket} = erlangzmq:socket(req),
    case erlangzmq:connect(Socket, tcp, "localhost", 5555, Resource) of
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
    case erlangzmq:send(Socket, Message) of
        ok ->
            io:format("Send message: ~p\n", [Message]);
        {error, Reason} ->
            io:format("Failed to send message: ~p, reason: ~p\n", [Message, Reason])
    end,
    case erlangzmq:recv(Socket) of
        {ok, RecvMessage} ->
            io:format("Recv message: ~p\n", [RecvMessage]);
        {error, RecvReason} ->
            io:format("Failed to recv, reason: ~p\n", [RecvReason])
    end,
    timer:sleep(1000),
    send_messages(Socket, Messages).
