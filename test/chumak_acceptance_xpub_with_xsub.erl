%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_xpub_with_xsub).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5587).

normal_test_() ->
    [
     {
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
    , {
       "Should allow to XPUB recv messages and XSUB send messages",
       {setup, fun start/0, fun stop/1, fun negociate_reverse_messages/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(xpub),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", ?PORT),
    Socket.

start_worker(Identity, SubscribeTopic, Func) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(xsub, Identity),
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

negotiate_subcriptions_without_multipart(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("XSUB-A", <<>>, NegociateFunc),
    start_worker("XSUB-B", <<>>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send(Socket, <<"Ready">>),

    MessageA = receive
        {recv, "XSUB-A", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "XSUB-B", MultipartB} ->
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
    start_worker("XSUB-C", <<>>, NegociateFunc),
    start_worker("XSUB-D", <<>>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"Ready">>, <<"OK">>]),

    MessageA = receive
        {recv, "XSUB-C", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "XSUB-D", MultipartB} ->
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
    start_worker("XSUB-E", <<"debug">>, NegociateFunc),
    start_worker("XSUB-F", <<"info">>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"debug">>,<<"DebugReady">>]),
    ok = chumak:send_multipart(Socket, [<<"info">>,<<"InfoReady">>]),

    MessageA = receive
        {recv, "XSUB-E", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "XSUB-F", MultipartB} ->
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
    start_worker("XSUB-G", <<"A">>, NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"A">>, <<"Message A">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"Message B">>]),

    Message1 = receive
        {recv, "XSUB-G", MultipartA} ->
            MultipartA
    end,
    timer:sleep(300), %% waits for reconnection
    ok = chumak:send_multipart(Socket, [<<"A">>, <<"Message A">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"Message B">>]),

    Message2 = receive
        {recv, "XSUB-G", MultipartB} ->
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
    start_worker("XSUB-H", <<"Z">>, NegociateFunc),

    %% waits the negotiation
    timer:sleep(400),
    ok = chumak:send_multipart(Socket, [<<"Z">>, <<"Message Z">>]),
    ok = chumak:send_multipart(Socket, [<<"W">>, <<"Message W">>]),

    Message = receive
        {recv, "XSUB-H", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual([<<"W">>, <<"Message W">>], Message)
    ].

negociate_reverse_messages(Socket) ->
    NegociateFunc = fun (ClientSocket, _Identity, _Parent) ->
                            ok = chumak:send_multipart(ClientSocket, [<<"hey girl <3">>])
                    end,
    start_worker("XSUB-I", <<"A">>, NegociateFunc),
    {ok, [Data]} = chumak:recv_multipart(Socket),

    [
     ?_assertEqual(<<"hey girl <3">>, Data)
    ].
