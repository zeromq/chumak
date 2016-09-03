%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_req_test).
-export([echo_rep_server/1]).
-include_lib("eunit/include/eunit.hrl").

req_single_test_() ->
    [
     {
       "Should send one message at time",
       {setup, fun start/0, fun stop/1, fun send_two_message_two_times/1}
     },
     {
       "When disconnected, should return no connected peers",
       {setup, fun start_without_connect/0, fun stop/1,
        fun send_message_without_connect/1}
     },
     {
       "When call send() and recv(), should recv message",
       {setup, fun start/0, fun stop/1,
        fun send_and_recv/1}
     },
     {
       "When call send() and more later call recv(), should recv message",
       {setup, fun start/0, fun stop/1,
        fun send_and_recv_with_delay/1}
     },
     {
       "When call send() and twice recv(), should deny the second recv",
       {setup, fun start/0, fun stop/1,
        fun send_and_twice_recv/1}
     },
     {
       "When call send() and twice recv() by distinct processes, should deny the second recv",
       {setup, fun start/0, fun stop/1,
        fun send_and_twice_recv_by_two_process/1}
     }
    ].

req_with_load_balancer_test_() ->
    [
     {
       "Should send and receive more than one message",
       {setup, fun start_with_lb/0, fun stop/1, fun send_and_receive_10_messages/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(req),
    ensure_echo_rep_server(rep_server1, 5555),
    ensure_echo_rep_server(rep_server2, 5556),

    {ok, _Client} = chumak:connect(Socket, tcp, "localhost", 5555),
    Socket.

ensure_echo_rep_server(Alias, Port) ->
    case whereis(Alias) of
        undefined ->
            {ok, Socket} = chumak:socket(rep, atom_to_list(Alias)),
            {ok, _BindProc} = chumak:bind(Socket, tcp, "127.0.0.1", Port),
            spawn(?MODULE, echo_rep_server, [Socket]),
            register(Alias, Socket),
            Socket;
        Pid ->
            Pid
    end.

echo_rep_server(Socket) ->
    {ok, Data} = chumak:recv(Socket),
    case Data of
        <<"delay">> ->
            timer:sleep(200);
        _ ->
            pass
    end,

    chumak:send(Socket, Data),
    echo_rep_server(Socket).

start_with_lb() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(req),

    ensure_echo_rep_server(rep_server1, 5555),
    ensure_echo_rep_server(rep_server2, 5556),

    {ok, _Client1} = chumak:connect(Socket, tcp, "localhost", 5555),
    {ok, _Client2} = chumak:connect(Socket, tcp, "localhost", 5556),
    Socket.


start_without_connect() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(req),
    Socket.

stop(Pid) ->
    gen_server:stop(Pid).

send_two_message_two_times(SocketPid)->
    [
     ?_assertEqual(chumak:send(SocketPid, <<"first message">>), ok),
     ?_assertEqual(chumak:send(SocketPid, <<"second message">>), {error, efsm})
    ].

send_message_without_connect(SocketPid) ->
    [
     ?_assertEqual(chumak:send(SocketPid, <<"first message">>), {error,no_connected_peers})
    ].

send_and_recv(SocketPid) ->
    [
     ?_assertEqual(chumak:send(SocketPid, "message"), ok),
     ?_assertEqual(chumak:recv(SocketPid), {ok, <<"message">>})
    ].

send_and_recv_with_delay(SocketPid) ->
    ok = chumak:send(SocketPid, <<"delayed message">>),
    ok = timer:sleep(200),
    [
     ?_assertEqual(chumak:recv(SocketPid), {ok, <<"delayed message">>})
    ].

send_and_twice_recv(SocketPid) ->
    [
     ?_assertEqual(chumak:send(SocketPid, <<"twice recv">>), ok),
     ?_assertEqual(chumak:recv(SocketPid), {ok, <<"twice recv">>}),
     ?_assertEqual(chumak:recv(SocketPid), {error, efsm})
    ].

send_and_twice_recv_by_two_process(SocketPid) ->
    ok = chumak:send(SocketPid, <<"delay">>),
    Parent = self(),

    spawn_link(fun () ->
                       Reply = chumak:recv(SocketPid),
                       timer:sleep(400),
                       Parent ! {recv_reply, Reply}
               end),
    {ok, <<"delay">>} = chumak:recv(SocketPid),
    AsyncReply = receive
                     {recv_reply, X} ->
                         X
                 end,
    [
     ?_assertEqual(AsyncReply, {error, efsm})
    ].

send_and_receive_10_messages(SocketPid) ->
    lists:map(
      fun (I) ->
              Message = io_lib:format("Message number ~p", [I]),
              MessageBin = list_to_binary(Message),
              ok = chumak:send(SocketPid, MessageBin),
              Reply = chumak:recv(SocketPid),
              ?_assertEqual(Reply, {ok, MessageBin})
      end, lists:seq(1, 10)).
