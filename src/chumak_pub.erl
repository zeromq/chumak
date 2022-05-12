%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Pub Pattern for Erlang
%%
%% This pattern implement Pub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc3

-module(chumak_pub).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, init/2, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, peer_subscribe/3, peer_cancel_subscribe/3,
         identity/1
        ]).

-record(chumak_pub, {
          identity         :: string(),
          subscriptions    :: #{PeerPid::pid => [Subscription::binary()]},
          xpub=false       :: false | true,
          recv_queue=nil   :: nil | {some, queue:queue()}, %% only for xpub
          pending_recv=nil :: nil | {from, From::term()}
         }).

valid_peer_type(sub)    -> valid;
valid_peer_type(xsub)   -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    init(Identity, []).

init(Identity, Opts) ->
    State = #chumak_pub{
               identity=Identity,
               subscriptions=chumak_subscriptions:new()
              },
    {ok, apply_opts(State, Opts)}.

terminate(_Reason, #chumak_pub{pending_recv=Recv}) ->
    case Recv of
        {from, From} -> gen_server:reply(From, {error, closed});
        _ -> ok
    end,

    ok.

identity(#chumak_pub{identity=I}) -> I.

apply_opts(State, []) ->
    State;

apply_opts(State, [xpub | Opts]) ->
    apply_opts(State#chumak_pub{
                 xpub=true,
                 recv_queue={some, queue:new()}
                }, Opts).

peer_flags(#chumak_pub{xpub=true}) ->
    {xpub, [incoming_queue]};

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

recv(#chumak_pub{xpub=true}=State, _From) ->
    {reply, {error, not_implemented_yet}, State};

recv(State, _From) ->
    {reply, {error, not_use}, State}.

send_multipart(#chumak_pub{subscriptions=Subscriptions}=State, Multipart, _From) ->
    [FirstPart | _] = Multipart,
    PeersPids = chumak_subscriptions:match(Subscriptions, FirstPart),

    lists:foreach(fun (PeerPid) ->
                          chumak_peer:send(PeerPid, Multipart)
                  end, PeersPids),

    {reply, ok, State}.

recv_multipart(#chumak_pub{pending_recv=nil, xpub=true, recv_queue={some, RecvQueue}}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#chumak_pub{recv_queue={some, NewRecvQueue}}};

        {empty, _RecvQueue} ->
            {noreply, State#chumak_pub{pending_recv={from, From}}}
    end;

recv_multipart(#chumak_pub{xpub=true}=State, _From) ->
    {reply, {error, efsm}, State};

recv_multipart(State, _From) ->
    {reply, {error, not_use}, State}.

unblock(#chumak_pub{pending_recv={from, PendingRecv}}=State, _From) ->
    NewState = State#chumak_pub{pending_recv=nil},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState};

unblock(#chumak_pub{pending_recv=nil}=State, _From) ->
    {reply, ok, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

queue_ready(#chumak_pub{xpub=true}=State, _Identity, PeerPid) ->
    case chumak_peer:incoming_queue_out(PeerPid) of
        {out, Multipart} ->
            {noreply,handle_queue_ready(State,Multipart)};
        empty ->
            {noreply,State};
        {error,Info}->
            ?LOG_WARNING("zmq queue error", #{error => send_error, reason => Info}),
            {noreply,State}
    end;

queue_ready(State, _Identity, _PeerPid) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

peer_disconected(#chumak_pub{subscriptions=Subscriptions}=State, PeerPid) ->
    NewSubscriptions = chumak_subscriptions:delete(Subscriptions, PeerPid),
    {noreply, State#chumak_pub{subscriptions=NewSubscriptions}}.

peer_subscribe(#chumak_pub{subscriptions=Subscriptions}=State, PeerPid, Subscription) ->
    NewSubscriptions = chumak_subscriptions:put(Subscriptions, PeerPid, Subscription),
    {noreply, State#chumak_pub{subscriptions=NewSubscriptions}}.

peer_cancel_subscribe(#chumak_pub{subscriptions=Subscriptions}=State, PeerPid, Subscription) ->
    NewSubscriptions = chumak_subscriptions:delete(Subscriptions, PeerPid, Subscription),
    {noreply, State#chumak_pub{subscriptions=NewSubscriptions}}.

handle_queue_ready(#chumak_pub{xpub=true, pending_recv=nil, recv_queue={some, RecvQueue}}=State,Data)->
    %% queue ready for XPUB pattern
    NewRecvQueue = queue:in(Data, RecvQueue),
    State#chumak_pub{recv_queue={some, NewRecvQueue}};

handle_queue_ready(#chumak_pub{xpub=true, pending_recv={from, PendingRecv}}=State, Data)->
    gen_server:reply(PendingRecv, {ok, Data}),
    State#chumak_pub{pending_recv=nil}.
