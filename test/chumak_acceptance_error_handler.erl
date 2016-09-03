%% @copyright 2016 Choven Corp.
%%
%% This file is part of chumak.
%%
%% chumak is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% chumak is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with chumak.  If not, see <http://www.gnu.org/licenses/>

-module(chumak_acceptance_error_handler).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 3010).

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
     ?_assertEqual(Message, {server_error, "Invalid socket-type push for rep server"})
    ].

wait_for_msg() ->
    receive
        {peer_finish, Msg} ->
            Msg
    end.
