%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_router_with_req).
-include_lib("eunit/include/eunit.hrl").

single_test_() ->
    [
     {
       "Should route message with two REQ",
       {setup, fun start/0, fun stop/1, fun negotiate_multiparts/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(router),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", 5576),
    Socket.

start_worker(Identity) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(req, Identity),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", 5576),
              worker_loop(Socket, Identity, Parent)
      end
     ).

worker_loop(Socket, Identity, Parent) ->
    chumak:send(Socket, <<"ready">>),
    {ok, Message} = chumak:recv(Socket),
    Parent ! {recv, Identity, Message}.


stop(Pid) ->
    gen_server:stop(Pid).


negotiate_multiparts(Socket) ->
    start_worker("REQ-A"),
    start_worker("REQ-B"),
    recv_and_send_message(Socket),
    timer:sleep(200),
    recv_and_send_message(Socket),

    MessageA = receive
        {recv, "REQ-A", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "REQ-B", MultipartB} ->
            MultipartB
    end,

    [
     ?_assertEqual(MessageA, <<"Reply: REQ-A">>),
     ?_assertEqual(MessageB, <<"Reply: REQ-B">>)
    ].

recv_and_send_message(Socket) ->
    {ok, [Identity, <<>>, <<"ready">>]} = chumak:recv_multipart(Socket),
    ok = chumak:send_multipart(Socket, [Identity, <<>>, <<"Reply: ", Identity/binary>>]).
