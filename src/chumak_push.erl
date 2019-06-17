%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Push Pattern for Erlang
%%
%% This pattern implement Push especification
%% from: http://rfc.zeromq.org/spec:30/PIPELINE#toc3

-module(chumak_push).
-behaviour(chumak_pattern).

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1
        ]).

-record(chumak_push, {
          identity         :: string(),
          lb               :: list()
         }).

valid_peer_type(pull)    -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #chumak_push{
               identity=Identity,
               lb=chumak_lb:new()
              },
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

identity(#chumak_push{identity=Identity}) -> Identity.

peer_flags(_State) ->
    {push, []}.

accept_peer(State, PeerPid) ->
    NewLb = chumak_lb:put(State#chumak_push.lb, PeerPid),
    {reply, {ok, PeerPid}, State#chumak_push{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(State, From) ->
    recv_multipart(State, From).

send_multipart(#chumak_push{lb=LB}=State, Multipart, From) ->
    case chumak_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            chumak_peer:send(PeerPid, Multipart, From),
            {noreply, State#chumak_push{lb=NewLB}}
    end.

recv_multipart(State, _From) ->
    {reply, {error, not_use}, State}.

unblock(State, _From) ->
    {reply, {error, not_use}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PUSH not receive messages
    {noreply, State}.

queue_ready(State, _Identity, _PeerPid) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

peer_disconected(#chumak_push{lb=LB}=State, PeerPid) ->
    NewLB = chumak_lb:delete(LB, PeerPid),
    {noreply, State#chumak_push{lb=NewLB}}.
