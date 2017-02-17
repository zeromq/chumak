%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_iron_house_test).
-include_lib("eunit/include/eunit.hrl").

-define(MESSAGE, <<"message from client">>).


authentication_test_() ->
    [
     {
       "Authenticated",
       {setup, fun start_authenticated/0, fun stop/1, fun push_and_pull/1}
     }
    , {
       "No authentication",
       {setup, fun start_no_authentication/0, fun stop/1, fun push_and_pull/1}
     }
    , {
       "Not authenticated (should fail)",
       {setup, fun start_not_authenticated/0, fun stop/1, fun negotiate_messages/1}
     }
    ].

start_authenticated() ->
    start(true).

start_no_authentication() ->
    start(any).

start_not_authenticated() ->
    start(false).

start(Authenticated) ->
    #{public := ServerPK, secret := ServerSK} = chumak_curve_if:box_keypair(),
    #{public := ClientPK, secret := ClientSK} = chumak_curve_if:box_keypair(),
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(pull),
    ok = chumak:set_socket_option(Socket, curve_server, true),
    ok = chumak:set_socket_option(Socket, curve_secretkey, ServerSK),
    case Authenticated of
        true ->
            ok = chumak:set_socket_option(Socket, curve_clientkeys, [ClientPK]);
        false ->
            RandomKey = chumak_curve_if:randombytes(32),
            ok = chumak:set_socket_option(Socket, curve_clientkeys, [RandomKey]);
        any->
            ok = chumak:set_socket_option(Socket, curve_clientkeys, any)
    end,
    {ok, _BindProc} = chumak:bind(Socket, tcp, "127.0.0.1", 5655),
    {Socket, ServerPK, ClientPK, ClientSK}.


stop({Pid, _, _, _}) ->
    gen_server:stop(Pid).

push_and_pull({SocketPid, ServerKey, PublicKey, SecretKey})->
    push(ServerKey, PublicKey, SecretKey),
    {ok, ReceivedData} = chumak:recv(SocketPid),
    [
     ?_assertEqual(ReceivedData, ?MESSAGE)
    ].


push(ServerKey, PK, SK) ->
    spawn_link(fun () ->
                       {ok, ClientSocket} = chumak:socket(push),
                       ok = chumak:set_socket_option(ClientSocket, curve_server, false),
                       ok = chumak:set_socket_option(ClientSocket, curve_serverkey, ServerKey),
                       ok = chumak:set_socket_option(ClientSocket, curve_secretkey, SK),
                       ok = chumak:set_socket_option(ClientSocket, curve_publickey, PK),
                       {ok, _ClientPid} = chumak:connect(ClientSocket, tcp, "127.0.0.1", 5655),
                       ok = chumak:send(ClientSocket, ?MESSAGE)
               end).


negotiate_messages(Values) ->
    invalid_client(Values),
    Message = wait_for_msg(),
    [
     ?_assertEqual({server_error, {error, closed}}, Message)
    ].

wait_for_msg() ->
    receive
        {peer_finish, Msg} ->
            Msg
    end.

invalid_client({_SocketPid, ServerKey, PK, SK}) ->
    Parent = self(),
    spawn(
      fun () ->
              process_flag(trap_exit, true),
              {ok, Socket} = chumak:socket(push),
              ok = chumak:set_socket_option(Socket, curve_server, false),
              ok = chumak:set_socket_option(Socket, curve_serverkey, ServerKey),
              ok = chumak:set_socket_option(Socket, curve_secretkey, SK),
              ok = chumak:set_socket_option(Socket, curve_publickey, PK),
              {ok, PeerPid} = chumak:connect(Socket, tcp, "127.0.0.1", 5655),
              link(PeerPid), %% to wait modifications
              client_loop(Parent, PeerPid)
      end
     ).

client_loop(Parent, PeerPid) ->
    receive
        {'EXIT', PeerPid, {shutdown, Reason}} ->
            Parent ! {peer_finish, Reason}
    end.
