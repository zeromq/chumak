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
    #{public := ServerPK, secret := ServerSK} = chumak_curve_if:box_keypair(),
    #{public := ClientPK, secret := ClientSK} = chumak_curve_if:box_keypair(),
    ok = chumak:set_socket_option(Socket, curve_server, true),
    ok = chumak:set_socket_option(Socket, curve_secretkey, ServerSK),
    ok = chumak:set_socket_option(Socket, curve_clientkeys, [ClientPK]),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", 5575),
    {Socket, #{curve_serverkey => ServerPK, 
               curve_publickey => ClientPK, 
               curve_secretkey => ClientSK}}.

start_worker(Identity, CurveOptions) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(dealer, Identity),
              [chumak:set_socket_option(Socket, Option, Value) 
               || {Option, Value} <- maps:to_list(CurveOptions)],
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

stop({Pid, _}) ->
    gen_server:stop(Pid).


negotiate_multiparts({Socket, CurveOptions}) ->
    start_worker("A", CurveOptions),
    start_worker("B", CurveOptions),
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
