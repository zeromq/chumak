%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
       "recv and send with curve security",
       {setup, fun start_req_curve/0, fun stop_req/1, fun rep_recv_and_send/1}
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
    {Socket, #{}}.

start_req_curve() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(rep),
    #{public := ServerPK, secret := ServerSK} = chumak_curve_if:box_keypair(),
    #{public := ClientPK, secret := ClientSK} = chumak_curve_if:box_keypair(),
    ok = chumak:set_socket_option(Socket, curve_server, true),
    ok = chumak:set_socket_option(Socket, curve_secretkey, ServerSK),
    ok = chumak:set_socket_option(Socket, curve_clientkeys, any),
    {ok, _BindProc} = chumak:bind(Socket, tcp, "127.0.0.1", 5655),
    {Socket, #{curve_serverkey => ServerPK, 
               curve_publickey => ClientPK, 
               curve_secretkey => ClientSK}}.

start_req_reverse() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(rep),
    req_server(5755),
    timer:sleep(50), %% wait the connection
    {ok, _BindProc} = chumak:connect(Socket, tcp, "127.0.0.1", 5755),
    {Socket, #{}}.

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

stop_req({Pid, _}) ->
    gen_server:stop(Pid).

rep_recv_and_send({SocketPid, CurveOptions})->
    spy_client(CurveOptions),
    {ok, <<"message from client 1">>} = chumak:recv(SocketPid),
    ok = chumak:send(SocketPid, <<"reply from client 1">>),
    ReceivedData = receive
                       {peer_recv, Data} -> Data
                   end,
    [
     ?_assertEqual(ReceivedData, <<"reply from client 1">>)
    ].

recv_reverse({SocketPid, _}) ->
    {ok, Message1} = chumak:recv(SocketPid),
    ok = chumak:send(SocketPid, <<"continue">>),
    {ok, Message2} = chumak:recv(SocketPid),
    ok = chumak:send(SocketPid, <<"quit">>),
    [
     ?_assertEqual(Message1, <<"Reverse Hello">>),
     ?_assertEqual(Message2, <<"Reverse Hello">>)
    ].

rep_send_without_recv({SocketPid, _}) ->
    [
     ?_assertEqual(chumak:send(SocketPid, <<"ok">>), {error, efsm})
    ].

rep_twice_recv({SocketPid, CurveOptions}) ->
    spy_client(CurveOptions),
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

rep_bufferize({SocketPid, CurveOptions}) ->
    spy_client(CurveOptions, <<"message 1">>),
    spy_client(CurveOptions, <<"message 2">>),
    timer:sleep(250),
    {ok, ReceivedData1} = chumak:recv(SocketPid),
    chumak:send(SocketPid, <<"ok">>),

    {ok, ReceivedData2} = chumak:recv(SocketPid),
    chumak:send(SocketPid, <<"ok">>),
    [
     ?_assertEqual([<<"message 1">>, <<"message 2">>], lists:sort([ReceivedData1, ReceivedData2]))
    ].

req_with_dealers({SocketPid, _}) ->
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

spy_client(CurveOptions) ->
    spy_client(CurveOptions, <<"message from client 1">>).


spy_client(CurveOptions, Msg) ->
    Parent = self(),
    spawn_link(fun () ->
                       timer:sleep(100), %% wait socket to be acceptable
                       {ok, ClientSocket} = chumak:socket(req),
                       [chumak:set_socket_option(ClientSocket, Option, Value) 
                        || {Option, Value} <- maps:to_list(CurveOptions)],
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
