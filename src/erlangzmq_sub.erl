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

%% @doc ZeroMQ Sub Pattern for Erlang
%%
%% This pattern implement Sub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc5

-module(erlangzmq_sub).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, init/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, subscribe/2, cancel/2,
         peer_reconnected/2
        ]).

-record(erlangzmq_sub, {
          identity               :: string(),
          topics                 :: list(),
          peers                  :: list(),
          pending_recv=nil       :: nil | atom(),
          pending_recv_multipart :: nil | false | true,
          recv_queue             :: queue:queue(),
          xsub=false             :: true | false
         }).

valid_peer_type(pub)    -> valid;
valid_peer_type(xpub)   -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    init(Identity, []).

init(Identity, Opts) ->
    State = #erlangzmq_sub{
               identity=Identity,
               topics=[],
               peers=[],
               recv_queue=queue:new(),
               pending_recv=nil,
               pending_recv_multipart=nil
              },
    {ok, apply_opts(State, Opts)}.

apply_opts(State, []) ->
    State;
apply_opts(State, [xsub | Opts]) ->
    apply_opts(State#erlangzmq_sub{xsub=true}, Opts).

peer_flags(#erlangzmq_sub{xsub=true}) ->
    {xsub, [incomming_queue]};
peer_flags(_State) ->
    {sub, [incomming_queue]}.

accept_peer(#erlangzmq_sub{peers=Peers, topics=Topics}=State, PeerPid) ->
    send_subscriptions(Topics, PeerPid),
    NewPeers = [PeerPid | Peers],
    {reply, {ok, PeerPid}, State#erlangzmq_sub{peers=NewPeers}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#erlangzmq_sub{pending_recv=nil, recv_queue=RecvQueue}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            FullMsg = binary:list_to_bin(Multipart),
            {reply, {ok, FullMsg}, State#erlangzmq_sub{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_sub{pending_recv=From, pending_recv_multipart=false}}
    end;

recv(State, _From) ->
    {reply, {error, efsm}, State}.

send_multipart(#erlangzmq_sub{xsub=true, peers=Peers}=State, Multipart, _From) ->
    Traffic = erlangzmq_protocol:encode_message_multipart(Multipart),
    lists:foreach(fun (PeerPid) ->
                          erlangzmq_peer:send(PeerPid, Traffic)
                  end, Peers),
    {reply, ok, State};

send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_use}, State}.

recv_multipart(#erlangzmq_sub{pending_recv=nil, recv_queue=RecvQueue}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#erlangzmq_sub{recv_queue=NewRecvQueue}};
        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_sub{pending_recv=From, pending_recv_multipart=true}}
    end;

recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

queue_ready(#erlangzmq_sub{recv_queue=RecvQueue, pending_recv=nil}=State, _Identity, PeerPid) ->
    {out, Messages} = erlangzmq_peer:incomming_queue_out(PeerPid),
    NewRecvQueue = queue:in(Messages, RecvQueue),
    {noreply, State#erlangzmq_sub{recv_queue=NewRecvQueue}};

queue_ready(State, _Identity, PeerPid) ->
    #erlangzmq_sub{pending_recv=PendingRecv, pending_recv_multipart=IsPendingMultipart} = State,

    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),

    case IsPendingMultipart of
        true ->
            gen_server:reply(PendingRecv, {ok, Multipart});
        false ->
            FullMsg = binary:list_to_bin(Multipart),
            gen_server:reply(PendingRecv, {ok, FullMsg})
    end,
    {noreply, State#erlangzmq_sub{pending_recv=nil, pending_recv_multipart=nil}}.

peer_disconected(#erlangzmq_sub{peers=Peers}=State, PeerPid) ->
    NewPeers = lists:delete(PeerPid, Peers),
    {noreply, State#erlangzmq_sub{peers=NewPeers}}.

subscribe(#erlangzmq_sub{topics=Topics, peers=Peers}=State, Topic) ->
    send_subscription_to_peers(Topic, Peers),
    {noreply, State#erlangzmq_sub{topics=[Topic |Topics]}}.

cancel(#erlangzmq_sub{topics=Topics, peers=Peers}=State, Topic) ->
    send_cancel_subscription_to_peers(Topic, Peers),
    NewTopics = lists:delete(Topic, Topics),
    {noreply, State#erlangzmq_sub{topics=NewTopics}}.

peer_reconnected(#erlangzmq_sub{topics=Topics}=State, PeerPid) ->
    send_subscriptions(Topics, PeerPid),
    {noreply, State}.

%% PRIVATE API
send_subscriptions(Topics, PeerPid) ->
    lists:foreach(fun (Topic) ->
                           erlangzmq_peer:send_subscription(PeerPid, Topic)
                  end, Topics).

send_subscription_to_peers(Topic, Peers) ->
     lists:foreach(fun (PeerPid) ->
                          erlangzmq_peer:send_subscription(PeerPid, Topic)
                  end, Peers).

send_cancel_subscription_to_peers(Topic, Peers) ->
    lists:foreach(fun (PeerPid) ->
                          erlangzmq_peer:send_cancel_subscription(PeerPid, Topic)
                  end, Peers).
