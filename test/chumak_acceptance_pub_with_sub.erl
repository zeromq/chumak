%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_pub_with_sub).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5586).

normal_test_() ->
    [
     {
       "Should deny PUB to recv message",
       {setup, fun start/0, fun stop/1, fun deny_pub_to_receive/1}
     }
    , {
       "Should deny SUB to send message",
       {setup, fun start/0, fun stop/1, fun deny_sub_to_send/1}
     }
    , {
       "Should deliver message for all subscribers",
       {setup, fun start/0, fun stop/1, fun negotiate_subcriptions_without_multipart/1}
     }
    , {
       "Should deliver message for all subscribers using multipart",
       {setup, fun start/0, fun stop/1, fun negotiate_subcriptions_with_multipart/1}
     }
    , {
       "Should deliver message for all subscribers that matching with pattern",
       {setup, fun start/0, fun stop/1, fun negotiate_subcriptions_with_matching/1}
     }
     , {
       "Should resend subscriptions when reconnection occurred",
       {setup, fun start/0, fun stop/1, fun negotiate_subcriptions_with_reconnect/1}
     }
     , {
       "Should allow to cancel and make other subscriptions",
       {setup, fun start/0, fun stop/1, fun cancel_and_remake_subscriptions/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(pub),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", ?PORT),
    Socket.

start_worker(Identity, SubscribeTopic, Func) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(sub, Identity),
              chumak:subscribe(Socket, SubscribeTopic),
              {ok, PeerPid} = chumak:connect(Socket, tcp, "localhost", ?PORT),

              case erlang:fun_info(Func, arity) of
                  {arity, 3} ->
                      Func(Socket, Identity, Parent);
                  {arity, 4} ->
                      Func(Socket, PeerPid, Identity, Parent)
              end
      end
     ).

stop(Pid) ->
    gen_server:stop(Pid).

deny_pub_to_receive(Socket) ->
    R1 = chumak:recv(Socket),
    R2 = chumak:recv_multipart(Socket),

    [
     ?_assertEqual({error, not_use}, R1),
     ?_assertEqual({error, not_use}, R2)
    ].

deny_sub_to_send(_Socket) ->
    {ok, ClientSocket} = chumak:socket(sub),
    {ok, _PeerPid} = chumak:connect(ClientSocket, tcp, "localhost", ?PORT),

    R1 = chumak:send(ClientSocket, <<"oi">>),
    R2 = chumak:send(ClientSocket, <<"oi">>),
    [
     ?_assertEqual({error, not_use}, R1),
     ?_assertEqual({error, not_use}, R2)
    ].

negotiate_subcriptions_without_multipart(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-A", <<>>, NegociateFunc),
    start_worker("SUB-B", <<>>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send(Socket, <<"Ready">>),

    MessageA = receive
        {recv, "SUB-A", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "SUB-B", MultipartB} ->
            MultipartB
    end,

    [
     ?_assertEqual(MessageA, <<"Ready">>),
     ?_assertEqual(MessageB, <<"Ready">>)
    ].

negotiate_subcriptions_with_multipart(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-C", <<>>, NegociateFunc),
    start_worker("SUB-D", <<>>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"Ready">>, <<"OK">>]),

    MessageA = receive
        {recv, "SUB-C", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "SUB-D", MultipartB} ->
            MultipartB
    end,

    [
     ?_assertEqual(MessageA, [<<"Ready">>, <<"OK">>]),
     ?_assertEqual(MessageB, [<<"Ready">>, <<"OK">>])
    ].

negotiate_subcriptions_with_matching(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-E", <<"debug">>, NegociateFunc),
    start_worker("SUB-F", <<"info">>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"debug">>,<<"DebugReady">>]),
    ok = chumak:send_multipart(Socket, [<<"info">>,<<"InfoReady">>]),

    MessageA = receive
        {recv, "SUB-E", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "SUB-F", MultipartB} ->
            MultipartB
    end,

    [
     ?_assertEqual([<<"debug">>, <<"DebugReady">>], MessageA),
     ?_assertEqual([<<"info">>, <<"InfoReady">>], MessageB)
    ].

negotiate_subcriptions_with_reconnect(Socket) ->
    NegociateFunc = fun (ClientSocket, PeerPid, Identity, Parent) ->
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message},
                            chumak_peer:reconnect(PeerPid),
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-G", <<"A">>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"A">>, <<"Message A">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"Message B">>]),

    Message1 = receive
        {recv, "SUB-G", MultipartA} ->
            MultipartA
    end,
    timer:sleep(300), %% waits for reconnection
    ok = chumak:send_multipart(Socket, [<<"A">>, <<"Message A">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"Message B">>]),

    Message2 = receive
        {recv, "SUB-G", MultipartB} ->
            MultipartB
    end,

    [
     ?_assertEqual([<<"A">>, <<"Message A">>], Message1),
     ?_assertEqual([<<"A">>, <<"Message A">>], Message2)
    ].

cancel_and_remake_subscriptions(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            chumak:cancel(ClientSocket, <<"Z">>),
                            chumak:subscribe(ClientSocket, <<"W">>),
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-H", <<"Z">>, NegociateFunc),

    %% waits the negotiation
    timer:sleep(400),
    ok = chumak:send_multipart(Socket, [<<"Z">>, <<"Message Z">>]),
    ok = chumak:send_multipart(Socket, [<<"W">>, <<"Message W">>]),

    Message = receive
        {recv, "SUB-H", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual([<<"W">>, <<"Message W">>], Message)
    ].
