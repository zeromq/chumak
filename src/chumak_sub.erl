%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Sub Pattern for Erlang
%%
%% This pattern implement Sub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc5

-module(chumak_sub).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, init/2, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, subscribe/2, cancel/2,
         peer_reconnected/2, identity/1
        ]).

-record(chumak_sub, {
          identity               :: string(),
          topics                 :: list(),
          peers                  :: list(),
          pending_recv=nil       :: nil | {from, From::term()},
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
    State = #chumak_sub{
               identity=Identity,
               topics=[],
               peers=[],
               recv_queue=queue:new(),
               pending_recv=nil,
               pending_recv_multipart=nil
              },
    {ok, apply_opts(State, Opts)}.

terminate(_Reason, #chumak_sub{pending_recv=Recv}) ->
    case Recv of
        {from, From} -> gen_server:reply(From, {error, closed});
        _ -> ok
    end,
    ok.

apply_opts(State, []) ->
    State;
apply_opts(State, [xsub | Opts]) ->
    apply_opts(State#chumak_sub{xsub=true}, Opts).

identity(#chumak_sub{identity=Identity}) -> Identity.

peer_flags(#chumak_sub{xsub=true}) ->
    {xsub, [incoming_queue]};
peer_flags(_State) ->
    {sub, [incoming_queue]}.

accept_peer(#chumak_sub{peers=Peers}=State, PeerPid) ->
    NewPeers = [PeerPid | Peers],
    {reply, {ok, PeerPid}, State#chumak_sub{peers=NewPeers}}.

peer_ready(#chumak_sub{topics=Topics}=State, PeerPid, _Identity) ->
    send_subscriptions(Topics, PeerPid),
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#chumak_sub{pending_recv=nil, recv_queue=RecvQueue}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            FullMsg = binary:list_to_bin(Multipart),
            {reply, {ok, FullMsg}, State#chumak_sub{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#chumak_sub{pending_recv={from, From}, pending_recv_multipart=false}}
    end;

recv(State, _From) ->
    {reply, {error, efsm}, State}.

unblock(#chumak_sub{pending_recv={from, PendingRecv}}=State, _From) ->
    NewState = State#chumak_sub{pending_recv=nil, pending_recv_multipart=nil},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState};

unblock(#chumak_sub{pending_recv=nil}=State, _From) ->
    {reply, ok, State}.

send_multipart(#chumak_sub{xsub=true, peers=Peers}=State, Multipart, _From) ->
    lists:foreach(fun (PeerPid) ->
                          chumak_peer:send(PeerPid, Multipart)
                  end, Peers),
    {reply, ok, State};

send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_use}, State}.

recv_multipart(#chumak_sub{pending_recv=nil, recv_queue=RecvQueue}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#chumak_sub{recv_queue=NewRecvQueue}};
        {empty, _RecvQueue} ->
            {noreply, State#chumak_sub{pending_recv={from, From}, pending_recv_multipart=true}}
    end;

recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

queue_ready(State, _Identity, PeerPid) ->
    case chumak_peer:incoming_queue_out(PeerPid) of
        {out, Multipart} ->
            {noreply,handle_queue_ready(State,Multipart)};
        empty ->
            {noreply,State};
        {error,Info}->
            ?LOG_ERROR("zmq queue error", #{error => process, type => sub, reason => Info}),
            {noreply,State}
    end.

peer_disconected(#chumak_sub{peers=Peers}=State, PeerPid) ->
    NewPeers = lists:delete(PeerPid, Peers),
    {noreply, State#chumak_sub{peers=NewPeers}}.

subscribe(#chumak_sub{topics=Topics, peers=Peers}=State, Topic) ->
    send_subscription_to_peers(Topic, Peers),
    {noreply, State#chumak_sub{topics=[Topic |Topics]}}.

cancel(#chumak_sub{topics=Topics, peers=Peers}=State, Topic) ->
    send_cancel_subscription_to_peers(Topic, Peers),
    NewTopics = lists:delete(Topic, Topics),
    {noreply, State#chumak_sub{topics=NewTopics}}.

peer_reconnected(#chumak_sub{topics=Topics}=State, PeerPid) ->
    send_subscriptions(Topics, PeerPid),
    {noreply, State}.

%% PRIVATE API
send_subscriptions(Topics, PeerPid) ->
    lists:foreach(fun (Topic) ->
                           chumak_peer:send_subscription(PeerPid, Topic)
                  end, Topics).

send_subscription_to_peers(Topic, Peers) ->
     lists:foreach(fun (PeerPid) ->
                          chumak_peer:send_subscription(PeerPid, Topic)
                  end, Peers).

send_cancel_subscription_to_peers(Topic, Peers) ->
    lists:foreach(fun (PeerPid) ->
                          chumak_peer:send_cancel_subscription(PeerPid, Topic)
                  end, Peers).

handle_queue_ready(#chumak_sub{recv_queue=RecvQueue, pending_recv=nil}=State,Data)->
    NewRecvQueue = queue:in(Data, RecvQueue),
    State#chumak_sub{recv_queue=NewRecvQueue};

handle_queue_ready(#chumak_sub{pending_recv={from, PendingRecv},
        pending_recv_multipart=IsPendingMultipart} = State, Data)->
    case IsPendingMultipart of
        true ->
            gen_server:reply(PendingRecv, {ok, Data});
        false ->
            FullMsg = binary:list_to_bin(Data),
            gen_server:reply(PendingRecv, {ok, FullMsg})
    end,
    State#chumak_sub{pending_recv=nil, pending_recv_multipart=nil}.
