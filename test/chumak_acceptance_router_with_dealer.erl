%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_router_with_dealer).
-include_lib("eunit/include/eunit.hrl").

single_test_() ->
    [
     {
       "Should route message with two DEALERS",
       {setup, fun start/0, fun stop/1, fun negotiate_multiparts/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(router),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", 5575),
    Socket.

start_worker(Identity) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(dealer, Identity),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", 5575),
              worker_loop(Socket, Identity, Parent)
      end
     ).

worker_loop(Socket, Identity, Parent) ->
    {ok, Multipart} = chumak:recv_multipart(Socket),
    case Multipart of
        [<<"EXIT">>] ->
            ok;
        _ ->
            Parent ! {recv, Identity, Multipart}
    end.

stop(Pid) ->
    gen_server:stop(Pid).


negotiate_multiparts(Socket) ->
    start_worker("A"),
    start_worker("B"),
    timer:sleep(200), %% wait client sockets to be estabilished
    ok = chumak:send_multipart(Socket, [<<"A">>, <<"My message one">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"My message two">>]),

    ok = chumak:send_multipart(Socket, [<<"A">>, <<"EXIT">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"EXIT">>]),

    MessageA = receive
        {recv, "A", MultipartA} ->
            MultipartA
    end,

    MessageB = receive
        {recv, "B", MultipartB} ->
            MultipartB
    end,

    [
     ?_assertEqual(MessageA, [<<"My message one">>]),
     ?_assertEqual(MessageB, [<<"My message two">>])
    ].
