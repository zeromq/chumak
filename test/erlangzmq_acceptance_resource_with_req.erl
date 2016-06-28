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

-module(erlangzmq_acceptance_resource_with_req).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 2710).

single_test_() ->
    [
     {
       "Should route message with two distinct servers",
       {setup, fun start/0, fun stop/1, fun negotiate_messages/1}
     }
    ].

start() ->
    application:ensure_started(erlangzmq),
    {ok, ResourceServerPid} = erlangzmq:resource(),
    {ok, _BindPid} = erlangzmq:bind(ResourceServerPid, tcp, "localhost", ?PORT),
    ResourceServerPid.

rep_server(ResourceServer, Resource, Reply) ->
    spawn_link(
      fun () ->
              {ok, Socket} = erlangzmq:socket(rep),
              erlangzmq:attach_resource(ResourceServer, Resource, Socket),
              rep_server_loop(Socket, Reply)
      end
     ).

rep_server_loop(Socket, Reply) ->
    {ok, Data} = erlangzmq:recv(Socket),
    erlangzmq:send(Socket, <<Reply/binary, ", ", Data/binary>>),
    rep_server_loop(Socket, Reply).


start_req_client(Identity, Resource, SendMsg) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = erlangzmq:socket(req),
              {ok, _PeerPid} = erlangzmq:connect(Socket, tcp, "localhost", ?PORT, Resource),
              erlangzmq:send(Socket, SendMsg),
              {ok, Message} = erlangzmq:recv(Socket),
              Parent ! {req_recv, Identity, Message}
      end
     ).


stop(Pid) ->
    gen_server:stop(Pid).


negotiate_messages(ResourceServerPid) ->
    rep_server(ResourceServerPid, "service/a", <<"Hello, I am the red ranger">>),
    rep_server(ResourceServerPid, "service/b", <<"Hello, I am the black ranger">>),

    timer:sleep(200),

    start_req_client("A", "service/a", <<"I am ready to rock">>),
    start_req_client("B", "service/b", <<"I am ready to beat">>),

    MessageA = wait_for_msg("A"),
    MessageB = wait_for_msg("B"),

    [
     ?_assertEqual(MessageA, <<"Hello, I am the red ranger, I am ready to rock">>),
     ?_assertEqual(MessageB, <<"Hello, I am the black ranger, I am ready to beat">>)
    ].

wait_for_msg(Id) ->
    receive
        {req_recv, Id, Message} ->
            Message
    end.
