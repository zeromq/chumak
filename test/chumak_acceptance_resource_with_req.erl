%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_resource_with_req).
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
    application:ensure_started(chumak),
    {ok, ResourceServerPid} = chumak:resource(),
    {ok, _BindPid} = chumak:bind(ResourceServerPid, tcp, "localhost", ?PORT),
    ResourceServerPid.

rep_server(ResourceServer, Resource, Reply) ->
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(rep),
              chumak:attach_resource(ResourceServer, Resource, Socket),
              rep_server_loop(Socket, Reply)
      end
     ).

rep_server_loop(Socket, Reply) ->
    {ok, Data} = chumak:recv(Socket),
    chumak:send(Socket, <<Reply/binary, ", ", Data/binary>>),
    rep_server_loop(Socket, Reply).


start_req_client(Identity, Resource, SendMsg) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(req),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", ?PORT, Resource),
              chumak:send(Socket, SendMsg),
              {ok, Message} = chumak:recv(Socket),
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
