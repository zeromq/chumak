%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.


%% @doc ZeroMQ Dealer Pattern for Erlang
%%
%% This pattern implement Dealer especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc5

-module(chumak_dealer).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1]).

-record(chumak_dealer, {
          identity          :: string(),
          lb                :: list(),
          pending_recv=none :: none | {from, From::term()},
          state=idle        :: idle | wait_req
         }).

% -define(RETRIES, 20).

valid_peer_type(rep)    -> valid;
valid_peer_type(router) -> valid;
valid_peer_type(dealer) -> valid;
valid_peer_type(_)      -> invalid.


init(Identity) ->
    State = #chumak_dealer{
               identity=Identity,
               lb=chumak_lb:new()
              },
    {ok, State}.

identity(#chumak_dealer{identity=I}) -> I.

terminate(_Reason, _State) ->
    ok.

peer_flags(_State) ->
    {dealer, [incoming_queue]}.

accept_peer(State, PeerPid) ->
    NewLb = chumak_lb:put(State#chumak_dealer.lb, PeerPid),
    {reply, {ok, PeerPid}, State#chumak_dealer{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, _Data, _From) ->
    {reply, {error, not_implemented_yet}, State}.

recv(State, _From) ->
    {reply, {error, not_implemented_yet}, State}.

send_multipart(#chumak_dealer{lb=LB}=State, Multipart, From) ->
    case chumak_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            chumak_peer:send(PeerPid, Multipart, From),
            {noreply, State#chumak_dealer{lb=NewLB}}
    end.

recv_multipart(#chumak_dealer{state=idle, lb=LB}=State, From) ->
    case chumak_lb:get(LB) of
        none ->
            {noreply, State#chumak_dealer{state=wait_req, pending_recv={from, From}}};
        {NewLB, PeerPid} ->
            direct_recv_multipart(State#chumak_dealer{lb=NewLB}, PeerPid, PeerPid, From)
    end;

recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use incoming_queue property
    {noreply, State}.

unblock(#chumak_dealer{pending_recv={from, PendingRecv}}=State, _From) ->
    NewState = State#chumak_dealer{pending_recv=none, state=idle},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState};

unblock(#chumak_dealer{state=idle}=State, _From) ->
    {reply, ok, State}.

queue_ready(#chumak_dealer{state=wait_req, pending_recv={from, PendingRecv}}=State, _Identity, PeerPid) ->
    FutureState =
        case chumak_peer:incoming_queue_out(PeerPid) of
            {out, Messages} ->
                gen_server:reply(PendingRecv, {ok, Messages}),
                State#chumak_dealer{state=idle, pending_recv=none};
            empty ->
                gen_server:reply(PendingRecv, {error, queue_empty}),
                State#chumak_dealer{state=idle, pending_recv=none};
            {error,Info}->
                ?LOG_WARNING("zmq queue error", #{error => cannot_process, reason => Info}),
                State
        end,
    {noreply, FutureState};

queue_ready(State, _Identity, _PeerPid) ->
    {noreply, State}.

peer_disconected(#chumak_dealer{lb=LB}=State, PeerPid) ->
    NewLB = chumak_lb:delete(LB, PeerPid),
    {noreply, State#chumak_dealer{lb=NewLB}}.

%% implement direct recv from peer queues
direct_recv_multipart(#chumak_dealer{lb=LB}=State, FirstPeerPid, PeerPid, From) ->
    case chumak_peer:incoming_queue_out(PeerPid) of
        {out, Messages} ->
            {reply, {ok, Messages}, State};

        {error, {timeout, _}} ->
          {reply, {error, timeout}, State};

        empty ->
            case chumak_lb:get(LB) of
                {NewLB, FirstPeerPid} ->
                    {noreply, State#chumak_dealer{state=wait_req, pending_recv={from, From}, lb=NewLB}};
                {NewLB, OtherPeerPid} ->
                    direct_recv_multipart(State#chumak_dealer{lb=NewLB}, FirstPeerPid, OtherPeerPid, From)
            end
    end.
