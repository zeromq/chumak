%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Router Pattern for Erlang
%%
%% This pattern implement Router especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc6

-module(chumak_router).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1]).

-record(chumak_router, {
          identity     :: string(),
          lbs,                         %% loadbalancers based on identity
          pending_recv :: nil | {from, From::term()},
          recv_queue   :: queue:queue()
}).

valid_peer_type(req)    -> valid;
valid_peer_type(router) -> valid;
valid_peer_type(dealer) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #chumak_router{
               identity=Identity,
               lbs=chumak_lbs:new(),
               recv_queue=queue:new(),
               pending_recv=nil
              },
    {ok, State}.

terminate(_Reason, #chumak_router{pending_recv=Recv}) ->
    case Recv of
        {from, From} -> gen_server:reply(From, {error, closed});
        _ -> ok
    end,
    ok.

identity(#chumak_router{identity=I}) -> I.

peer_flags(_State) ->
    {router, [incoming_queue]}.

accept_peer(State, PeerPid) ->
    {reply, {ok, PeerPid}, State}.

peer_ready(#chumak_router{lbs=LBs}=State, PeerPid, Identity) ->
    NewLBs = chumak_lbs:put(LBs, Identity, PeerPid),
    {noreply, State#chumak_router{lbs=NewLBs}}.

send(State, _Data, _From) ->
    {reply, {error, send_implemented_yet}, State}.

recv(State, _From) ->
    {reply, {error, recv_implemented_yet}, State}.

send_multipart(#chumak_router{lbs=LBs}=State, Multipart, _From) when length(Multipart) >= 2 ->
    [Identity|RemmaingMultipart] = Multipart,
    case chumak_lbs:get(LBs, binary_to_list(Identity)) of
        {NewLBs, PeerPid} ->
            chumak_peer:send(PeerPid, RemmaingMultipart),
            {reply, ok, State#chumak_router{lbs=NewLBs}};

        none ->
           {reply, {error, no_peers}, State}
    end;

send_multipart(State, _Multipart, _From) ->
    {reply, {error, identity_missing}, State}.

recv_multipart(#chumak_router{recv_queue=RecvQueue, pending_recv=nil}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#chumak_router{recv_queue=NewRecvQueue}};
        {empty, _RecvQueue} ->
            {noreply, State#chumak_router{pending_recv={from, From}}}
    end;
recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

unblock(#chumak_router{pending_recv={from, PendingRecv}}=State, _From) ->
    NewState = State#chumak_router{pending_recv=nil},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState};

unblock(#chumak_router{pending_recv=nil}=State, _From) ->
    {reply, ok, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use incoming_queue property
    {noreply, State}.

queue_ready(State, Identity, PeerPid) ->
    case chumak_peer:incoming_queue_out(PeerPid) of
        {out, Multipart} ->
            IdentityBin = list_to_binary(Identity),
            {noreply,handle_queue_ready(State,[IdentityBin | Multipart])};
        empty ->
            {noreply,State};
        {error,Info}->
            ?LOG_WARNING("zmq queue error", #{error => send_error, type => router, reason => Info}),
            {noreply,State}
    end.

peer_disconected(#chumak_router{lbs=LBs}=State, PeerPid) ->
    NewLBs = chumak_lbs:delete(LBs, PeerPid),
    {noreply, State#chumak_router{lbs=NewLBs}}.

handle_queue_ready(#chumak_router{recv_queue=RecvQueue, pending_recv=nil}=State,Data)->
    NewRecvQueue = queue:in(Data, RecvQueue),
    State#chumak_router{recv_queue=NewRecvQueue};

handle_queue_ready(#chumak_router{pending_recv={from, PendingRecv}}=State, Data)->
    gen_server:reply(PendingRecv, {ok, Data}), %% if there is a waiter reply directly
    State#chumak_router{pending_recv=nil}.
