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

%% @doc ZeroMQ Pub Pattern for Erlang
%%
%% This pattern implement Pub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc3

-module(erlangzmq_pub).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, init/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, peer_subscribe/3, peer_cancel_subscribe/3
        ]).

-record(erlangzmq_pub, {
          identity         :: string(),
          subscriptions    :: #{PeerPid::pid => [Subscription::binary()]},
          xpub=false       :: false | true,
          recv_queue       :: queue:queue(), %% only for xpub
          pending_recv=nil :: nil | atom()
         }).

valid_peer_type(sub)    -> valid;
valid_peer_type(xsub)   -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    init(Identity, []).

init(Identity, Opts) ->
    State = #erlangzmq_pub{
               identity=Identity,
               subscriptions=erlangzmq_subscriptions:new()
              },
    {ok, apply_opts(State, Opts)}.

apply_opts(State, []) ->
    State;

apply_opts(State, [xpub | Opts]) ->
    apply_opts(State#erlangzmq_pub{
                 xpub=true,
                 recv_queue=queue:new()
                }, Opts).

peer_flags(#erlangzmq_pub{xpub=true}) ->
    {xpub, [incomming_queue]};

peer_flags(_State) ->
    %% pub_compatible_layer is used to allow decoder to understand old style of subscription
    %% in the 3.0 version of ZeroMQ.
    {pub, [pub_compatible_layer]}.

accept_peer(State, PeerPid) ->
    {reply, {ok, PeerPid}, State}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#erlangzmq_pub{xpub=true}=State, _From) ->
    {reply, {error, not_implemented_yet}, State};

recv(State, _From) ->
    {reply, {error, not_use}, State}.

send_multipart(#erlangzmq_pub{subscriptions=Subscriptions}=State, Multipart, _From) ->
    [FirstPart | _] = Multipart,
    Traffic = erlangzmq_protocol:encode_message_multipart(Multipart),
    PeersPids = erlangzmq_subscriptions:match(Subscriptions, FirstPart),

    lists:foreach(fun (PeerPid) ->
                          erlangzmq_peer:send(PeerPid, Traffic)
                  end, PeersPids),

    {reply, ok, State}.

recv_multipart(#erlangzmq_pub{pending_recv=nil, xpub=true}=State, From) ->
    case queue:out(State#erlangzmq_pub.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#erlangzmq_pub{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_pub{pending_recv=From}}
    end;

recv_multipart(#erlangzmq_pub{xpub=true}=State, _From) ->
    {reply, {error, efsm}, State};

recv_multipart(State, _From) ->
    {reply, {error, not_use}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

queue_ready(#erlangzmq_pub{xpub=true, pending_recv=nil}=State, _Identity, PeerPid) ->
    %% queue ready for XPUB pattern
    {out, Messages} = erlangzmq_peer:incomming_queue_out(PeerPid),
    NewRecvQueue = queue:in(Messages, State#erlangzmq_pub.recv_queue),
    {noreply, State#erlangzmq_pub{recv_queue=NewRecvQueue}};

queue_ready(#erlangzmq_pub{xpub=true, pending_recv=PendingRecv}=State, _Identity, PeerPid) ->
    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),
    gen_server:reply(PendingRecv, {ok, Multipart}),
    {noreply, State#erlangzmq_pub{pending_recv=nil}};

queue_ready(State, _Identity, _PeerPid) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

peer_disconected(#erlangzmq_pub{subscriptions=Subscriptions}=State, PeerPid) ->
    NewSubscriptions = erlangzmq_subscriptions:delete(Subscriptions, PeerPid),
    {noreply, State#erlangzmq_pub{subscriptions=NewSubscriptions}}.

peer_subscribe(#erlangzmq_pub{subscriptions=Subscriptions}=State, PeerPid, Subscription) ->
    NewSubscriptions = erlangzmq_subscriptions:put(Subscriptions, PeerPid, Subscription),
    {noreply, State#erlangzmq_pub{subscriptions=NewSubscriptions}}.

peer_cancel_subscribe(#erlangzmq_pub{subscriptions=Subscriptions}=State, PeerPid, Subscription) ->
    NewSubscriptions = erlangzmq_subscriptions:delete(Subscriptions, PeerPid, Subscription),
    {noreply, State#erlangzmq_pub{subscriptions=NewSubscriptions}}.
