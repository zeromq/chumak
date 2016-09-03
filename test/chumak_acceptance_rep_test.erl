%% @copyright 2016 Choven Corp.
%%
%% This file is part of chumak.
%%
%% chumak is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% chumak is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with chumak.  If not, see <http://www.gnu.org/licenses/>

-module(chumak_acceptance_rep_test).
-export([echo_req_server/1]).
-include_lib("eunit/include/eunit.hrl").

rep_single_test_() ->
    [
     {
       "Should block recv until one peer send message",
       {setup, fun start_req/0, fun stop_req/1, fun rep_recv_and_send/1}
     },
     {
       "Should deny send without received a message",
       {setup, fun start_req/0, fun stop_req/1, fun rep_send_without_recv/1}
     },
     {
       "Should deny twice recvs",
       {setup, fun start_req/0, fun stop_req/1, fun rep_twice_recv/1}
     },
     {
       "Should bufferize while sent is not called yet",
       {setup, fun start_req/0, fun stop_req/1, fun rep_bufferize/1}
     }
    ].

rep_reverse_test_() ->
    [
     {
       "Should connect to req peer",
       {setup, fun start_req_reverse/0, fun stop_req/1, fun recv_reverse/1}
     }
    ].

rep_with_dealer_test_() ->
    [
     {
       "Should connect to dealer peer",
       {setup, fun start_req/0, fun stop_req/1, fun req_with_dealers/1}
     }
    ].

start_req() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(rep),
    {ok, _BindProc} = chumak:bind(Socket, tcp, "127.0.0.1", 5655),
    Socket.

start_req_reverse() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(rep),
    req_server(5755),
    timer:sleep(50), %% wait the connection
    {ok, _BindProc} = chumak:connect(Socket, tcp, "127.0.0.1", 5755),
    Socket.

req_server(Port) ->
    {ok, Socket} = chumak:socket(req),
    {ok, _BindProc} = chumak:bind(Socket, tcp, "127.0.0.1", Port),
    spawn(?MODULE, echo_req_server, [Socket]).

echo_req_server(Socket) ->
    timer:sleep(100), %% wait for a peer connect
    ok = chumak:send(Socket, <<"Reverse Hello">>),
    {ok, Message} = chumak:recv(Socket),

    case Message of
        <<"quit">> ->
            quit;
        _ ->
            echo_req_server(Socket)
    end.

stop_req(Pid) ->
    gen_server:stop(Pid).

rep_recv_and_send(SocketPid)->
    spy_client(),
    {ok, <<"message from client 1">>} = chumak:recv(SocketPid),
    ok = chumak:send(SocketPid, <<"reply from client 1">>),
    ReceivedData = receive
                       {peer_recv, Data} -> Data
                   end,
    [
     ?_assertEqual(ReceivedData, <<"reply from client 1">>)
    ].

recv_reverse(SocketPid) ->
    {ok, Message1} = chumak:recv(SocketPid),
    ok = chumak:send(SocketPid, <<"continue">>),
    {ok, Message2} = chumak:recv(SocketPid),
    ok = chumak:send(SocketPid, <<"quit">>),
    [
     ?_assertEqual(Message1, <<"Reverse Hello">>),
     ?_assertEqual(Message2, <<"Reverse Hello">>)
    ].

rep_send_without_recv(SocketPid) ->
    [
     ?_assertEqual(chumak:send(SocketPid, <<"ok">>), {error, efsm})
    ].

rep_twice_recv(SocketPid) ->
    spy_client(),
    Parent = self(),
    spawn_link(fun () ->
                       timer:sleep(100),
                       Reply = chumak:recv(SocketPid),
                       Parent ! {recv_reply, Reply}
               end),
    {ok, ReceivedData} = chumak:recv(SocketPid),
    AsyncReply = receive
                     {recv_reply, X} ->
                         X
                 end,
    [
     ?_assertEqual(ReceivedData, <<"message from client 1">>),
     ?_assertEqual(AsyncReply, {error, efsm})
    ].

rep_bufferize(SocketPid) ->
    spy_client(<<"message 1">>),
    spy_client(<<"message 2">>),
    timer:sleep(250),
    {ok, ReceivedData1} = chumak:recv(SocketPid),
    chumak:send(SocketPid, <<"ok">>),

    {ok, ReceivedData2} = chumak:recv(SocketPid),
    chumak:send(SocketPid, <<"ok">>),
    [
     ?_assertEqual([<<"message 1">>, <<"message 2">>], lists:sort([ReceivedData1, ReceivedData2]))
    ].

req_with_dealers(SocketPid) ->
    dealer_client(),

    {ok, ReceivedData1} = chumak:recv(SocketPid),
    chumak:send(SocketPid, <<"ok 1">>),

    {ok, ReceivedData2} = chumak:recv(SocketPid),
    chumak:send(SocketPid, <<"ok 2">>),

    RecvMsgs = receive
                   {recv_msgs, Msgs} ->
                       Msgs
               end,

    [
     ?_assertEqual(<<"packet 1">>, ReceivedData1),
     ?_assertEqual(<<"packet 2">>, ReceivedData2),
     ?_assertEqual([[<<>>, <<"ok 1">>], [<<>>, <<"ok 2">>]], RecvMsgs)
    ].

spy_client() ->
    spy_client(<<"message from client 1">>).

spy_client(Msg) ->
    Parent = self(),
    spawn_link(fun () ->
                       timer:sleep(100), %% wait socket to be acceptable
                       {ok, ClientSocket} = chumak:socket(req),
                       {ok, _ClientPid} = chumak:connect(ClientSocket, tcp, "127.0.0.1", 5655),
                       ok = chumak:send(ClientSocket, Msg),
                       {ok, Data} = chumak:recv(ClientSocket),
                       Parent ! {peer_recv, Data}
               end).

dealer_client() ->
   Parent = self(),
    spawn_link(fun () ->
                       timer:sleep(100), %% wait socket to be acceptable
                       {ok, ClientSocket} = chumak:socket(dealer),
                       {ok, _ClientPid} = chumak:connect(ClientSocket, tcp, "127.0.0.1", 5655),
                       ok = chumak:send_multipart(ClientSocket, [<<>>, <<"packet 1">>]),
                       ok = chumak:send_multipart(ClientSocket, [<<>>, <<"packet 2">>]),

                       {ok, Message1} = chumak:recv_multipart(ClientSocket),
                       {ok, Message2} = chumak:recv_multipart(ClientSocket),

                       Parent ! {recv_msgs, [Message1, Message2]}


               end).
