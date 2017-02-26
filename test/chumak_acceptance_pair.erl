%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_pair).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5587).

normal_test_() ->
    [
     {
       "Should deliver message for the paired peer",
       {setup, fun start/0, fun stop/1, fun negotiate_without_multipart/1}
     }
     , {
       "Should deliver message for the paired peer, encrypted",
       {setup, fun start_curve/0, fun stop/1, fun negotiate_without_multipart_curve/1}
     }
    , {
       "Should deliver message for the paired peer using multipart",
       {setup, fun start/0, fun stop/1, fun negotiate_with_multipart/1}
     }
    , {
       "Should deliver message for the paired peer without timeout",
       {setup, fun start/0, fun stop/1, fun negotiate_without_timeout/1}
     }
     , {
       "Should deliver message for the paired peer with delayed message",
       {setup, fun start/0, fun stop/1, fun negotiate_with_delay/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(pair),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", ?PORT),
    {Socket, #{}}.

start_curve() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(pair),
    #{public := ServerPK, secret := ServerSK} = chumak_curve_if:box_keypair(),
    #{public := ClientPK, secret := ClientSK} = chumak_curve_if:box_keypair(),
    ok = chumak:set_socket_option(Socket, curve_server, true),
    ok = chumak:set_socket_option(Socket, curve_secretkey, ServerSK),
    ok = chumak:set_socket_option(Socket, curve_clientkeys, any),
    {ok, _BindProc} = chumak:bind(Socket, tcp, "localhost", ?PORT),
    {Socket, #{curve_serverkey => ServerPK, 
               curve_publickey => ClientPK, 
               curve_secretkey => ClientSK}}.

start_worker(Identity,  Func, CurveOptions) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(pair, Identity),
              [chumak:set_socket_option(Socket, Option, Value) 
               || {Option, Value} <- maps:to_list(CurveOptions)],
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", ?PORT),
              Func(Socket, Identity, Parent)
      end
     ).

stop({Pid, _}) ->
    gen_server:stop(Pid).

negotiate_without_multipart({Socket, CurveOptions}) ->
    do_negotiate(Socket, CurveOptions, "PAIR-A").

negotiate_without_multipart_curve({Socket, CurveOptions}) ->
    do_negotiate(Socket, CurveOptions, "PAIR-AC").

do_negotiate(Socket, CurveOptions, Id) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker(Id, NegociateFunc, CurveOptions),
    timer:sleep(200),
    ok = chumak:send(Socket, <<"Hey brother">>),

    Message = receive
        {recv, Id, MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, <<"Hey brother">>)
    ].

negotiate_with_multipart({Socket, CurveOptions}) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-B", NegociateFunc, CurveOptions),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"Hey">>, <<"Jude">>]),

    Message = receive
        {recv, "PAIR-B", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, [<<"Hey">>, <<"Jude">>])
    ].

negotiate_without_timeout({Socket, CurveOptions}) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-C", NegociateFunc, CurveOptions),
    ok = chumak:send(Socket, <<"Hey mother">>),

    Message = receive
        {recv, "PAIR-C", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, <<"Hey mother">>)
    ].

negotiate_with_delay({Socket, CurveOptions}) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            timer:sleep(200),
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-D", NegociateFunc, CurveOptions),
    ok = chumak:send(Socket, <<"Hey father">>),

    Message = receive
        {recv, "PAIR-D", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, <<"Hey father">>)
    ].
