%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_stone_house_test).
-include_lib("eunit/include/eunit.hrl").

-define(MESSAGE, <<"message from client">>).

rep_single_test_() ->
    [
     {
       "push - pull with curve security",
       {setup, fun start_valid/0, fun stop/1, fun push_and_pull/1}
     }
    ].

start_valid() ->
    #{public := PK, secret := SK} = chumak_curve_if:box_keypair(),
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(pull),
    ok = chumak:set_socket_option(Socket, curve_server, true),
    ok = chumak:set_socket_option(Socket, curve_secretkey, SK),
    {ok, _BindProc} = chumak:bind(Socket, tcp, "127.0.0.1", 5655),
    {Socket, PK}.


stop({Pid, _}) ->
    gen_server:stop(Pid).

push_and_pull({SocketPid, ServerKey})->
    push(ServerKey),
    {ok, ReceivedData} = chumak:recv(SocketPid),
    [
     ?_assertEqual(ReceivedData, ?MESSAGE)
    ].

push(ServerKey) ->
    #{public := PK, secret := SK} = chumak_curve_if:box_keypair(),
    spawn_link(fun () ->
                       timer:sleep(100), %% wait socket to be acceptable
                       {ok, ClientSocket} = chumak:socket(push),
                       ok = chumak:set_socket_option(ClientSocket, curve_server, false),
                       ok = chumak:set_socket_option(ClientSocket, curve_serverkey, ServerKey),
                       ok = chumak:set_socket_option(ClientSocket, curve_secretkey, SK),
                       ok = chumak:set_socket_option(ClientSocket, curve_publickey, PK),
                       {ok, _ClientPid} = chumak:connect(ClientSocket, tcp, "127.0.0.1", 5655),
                       ok = chumak:send(ClientSocket, ?MESSAGE)
               end).
