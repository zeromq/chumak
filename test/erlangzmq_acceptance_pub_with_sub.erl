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

-module(erlangzmq_acceptance_pub_with_sub).
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
       "Should resend subscriptions when reconnection ocurred",
       {setup, fun start/0, fun stop/1, fun negotiate_subcriptions_with_reconnect/1}
     }
     , {
       "Should allow to cancel and make other subscriptions",
       {setup, fun start/0, fun stop/1, fun cancel_and_remake_subscriptions/1}
     }
    ].

start() ->
    application:ensure_started(erlangzmq),
    {ok, Socket} = erlangzmq:socket(pub),
    {ok, _BindPid} = erlangzmq:bind(Socket, tcp, "localhost", ?PORT),
    Socket.

start_worker(Identity, SubscribeTopic, Func) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = erlangzmq:socket(sub, Identity),
              erlangzmq:subscribe(Socket, SubscribeTopic),
              {ok, PeerPid} = erlangzmq:connect(Socket, tcp, "localhost", ?PORT),

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
    R1 = erlangzmq:recv(Socket),
    R2 = erlangzmq:recv_multipart(Socket),

    [
     ?_assertEqual({error, not_use}, R1),
     ?_assertEqual({error, not_use}, R2)
    ].

deny_sub_to_send(_Socket) ->
    {ok, ClientSocket} = erlangzmq:socket(sub),
    {ok, _PeerPid} = erlangzmq:connect(ClientSocket, tcp, "localhost", ?PORT),

    R1 = erlangzmq:send(ClientSocket, <<"oi">>),
    R2 = erlangzmq:send(ClientSocket, <<"oi">>),
    [
     ?_assertEqual({error, not_use}, R1),
     ?_assertEqual({error, not_use}, R2)
    ].

negotiate_subcriptions_without_multipart(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = erlangzmq:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-A", <<>>, NegociateFunc),
    start_worker("SUB-B", <<>>, NegociateFunc),
    timer:sleep(200),
    ok = erlangzmq:send(Socket, <<"Ready">>),

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
                            {ok, Message} = erlangzmq:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-C", <<>>, NegociateFunc),
    start_worker("SUB-D", <<>>, NegociateFunc),
    timer:sleep(200),
    ok = erlangzmq:send_multipart(Socket, [<<"Ready">>, <<"OK">>]),

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
                            {ok, Message} = erlangzmq:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-E", <<"debug">>, NegociateFunc),
    start_worker("SUB-F", <<"info">>, NegociateFunc),
    timer:sleep(200),
    ok = erlangzmq:send_multipart(Socket, [<<"debug">>,<<"DebugReady">>]),
    ok = erlangzmq:send_multipart(Socket, [<<"info">>,<<"InfoReady">>]),

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
                            {ok, Message} = erlangzmq:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message},
                            erlangzmq_peer:reconnect(PeerPid),
                            {ok, Message} = erlangzmq:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-G", <<"A">>, NegociateFunc),
    timer:sleep(200),
    ok = erlangzmq:send_multipart(Socket, [<<"A">>, <<"Message A">>]),
    ok = erlangzmq:send_multipart(Socket, [<<"B">>, <<"Message B">>]),

    Message1 = receive
        {recv, "SUB-G", MultipartA} ->
            MultipartA
    end,
    timer:sleep(300), %% waits for reconnection
    ok = erlangzmq:send_multipart(Socket, [<<"A">>, <<"Message A">>]),
    ok = erlangzmq:send_multipart(Socket, [<<"B">>, <<"Message B">>]),

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
                            erlangzmq:cancel(ClientSocket, <<"Z">>),
                            erlangzmq:subscribe(ClientSocket, <<"W">>),
                            {ok, Message} = erlangzmq:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("SUB-H", <<"Z">>, NegociateFunc),

    %% waits the negotiation
    timer:sleep(400),
    ok = erlangzmq:send_multipart(Socket, [<<"Z">>, <<"Message Z">>]),
    ok = erlangzmq:send_multipart(Socket, [<<"W">>, <<"Message W">>]),

    Message = receive
        {recv, "SUB-H", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual([<<"W">>, <<"Message W">>], Message)
    ].
