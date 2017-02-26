%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_error_handler).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 3011).

single_test_() ->
    [
     {
       "Should not start connection with invalid servers",
       {setup, fun start/0, fun stop/1, fun negotiate_messages/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, ServerPid} = chumak:socket(rep),
    {ok, _BindPid} = chumak:bind(ServerPid, tcp, "localhost", ?PORT),
    ServerPid.

invalid_client() ->
    Parent = self(),
    spawn(
      fun () ->
              process_flag(trap_exit, true),
              {ok, Socket} = chumak:socket(push),
              {ok, PeerPid} = chumak:connect(Socket, tcp, "localhost", ?PORT),
              link(PeerPid), %% to wait modifications
              client_loop(Parent, PeerPid)
      end
     ).

client_loop(Parent, PeerPid) ->
    receive
        {'EXIT', PeerPid, {shutdown, Reason}} ->
            Parent ! {peer_finish, Reason}
    end.

stop(Pid) ->
    gen_server:stop(Pid).


negotiate_messages(_ServerPid) ->
    invalid_client(),

    Message = wait_for_msg(),

    [
     ?_assertEqual({server_error, "Invalid socket-type push for rep server"}, Message)
    ].

wait_for_msg() ->
    receive
        {peer_finish, Msg} ->
            Msg
    end.
