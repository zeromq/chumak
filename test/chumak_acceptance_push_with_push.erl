%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_push_with_push).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5595).

push_test_() ->
    [
     {
       "Should send with round robin strategy",
       {setup, fun start/0, fun stop/1, fun send_round_robin/1}
     },
     {
       "Should send and recv multi_part",
       {setup, fun start/0, fun stop/1, fun send_multi_part/1}
     },
     {
       "When disconnected, should return no connected peers",
       {setup, fun start/0, fun stop/1,
        fun send_message_without_connect/1}
     },
     {
       "When call recv(), should deny recv message",
       {setup, fun start/0, fun stop/1,
        fun send_and_recv/1}
     }
    ].


start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(push),
    {ok, _Client} = chumak:bind(Socket, tcp, "localhost", ?PORT),
    Socket.

stop(Pid) ->
    gen_server:stop(Pid).

send_round_robin(SocketPid) ->
    Parent = self(),

    spawn_link(
      fun() ->
              {ok, Client} = chumak:socket(pull),
              {ok, _PeerPid} = chumak:connect(Client, tcp, "localhost", ?PORT),
              {ok, Data} = chumak:recv(Client),
              Parent ! {recv, 1, Data}
      end
     ),
    timer:sleep(100),
    spawn_link(
      fun() ->
              {ok, Client} = chumak:socket(pull),
              {ok, _PeerPid} = chumak:connect(Client, tcp, "localhost", ?PORT),
              {ok, Data} = chumak:recv(Client),
              Parent ! {recv, 2, Data}
      end
     ),
    timer:sleep(200),

    ok = chumak:send(SocketPid, <<"first message">>),
    ok = chumak:send(SocketPid, <<"second message">>),

    Message1 = receive
                   {recv, 1, Data1} ->
                       Data1
               end,
    Message2 = receive
                   {recv, 2, Data2} ->
                       Data2
               end,

    [
     ?_assertEqual(<<"first message">>, Message2),
     ?_assertEqual(<<"second message">>, Message1)
    ].

send_multi_part(SocketPid) ->
    Parent = self(),

    spawn_link(
      fun() ->
              {ok, Client} = chumak:socket(pull),
              {ok, _PeerPid} = chumak:connect(Client, tcp, "localhost", ?PORT),
              {ok, Data} = chumak:recv_multipart(Client),
              Parent ! {recv, 3, Data}
      end
     ),

    timer:sleep(100),
    spawn_link(
      fun() ->
              {ok, Client} = chumak:socket(pull),
              {ok, _PeerPid} = chumak:connect(Client, tcp, "localhost", ?PORT),
              {ok, Data} = chumak:recv_multipart(Client),
              Parent ! {recv, 4, Data}
      end
     ),

    timer:sleep(200),

    ok = chumak:send_multipart(SocketPid, [<<"Hey">>, <<"Joe">>]),
    ok = chumak:send_multipart(SocketPid, [<<"Lucy">>, <<"Sky">>]),

    Message1 = receive
                   {recv, 3, Data1} ->
                       Data1
               end,
    Message2 = receive
                   {recv, 4, Data2} ->
                       Data2
               end,

    [
     ?_assertEqual([<<"Lucy">>, <<"Sky">>], Message1),
     ?_assertEqual([<<"Hey">>, <<"Joe">>], Message2)
    ].


send_message_without_connect(SocketPid) ->
    [
     ?_assertEqual({error,no_connected_peers}, chumak:send(SocketPid, <<"first message">>))
    ].

send_and_recv(SocketPid) ->
    [
     ?_assertEqual({error,not_use}, chumak:recv(SocketPid))
    ].
