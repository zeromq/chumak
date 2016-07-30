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


%% @doc ZeroMQ Dealer Pattern for Erlang
%%
%% This pattern implement Dealer especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc5

-module(erlangzmq_dealer).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2]).

-record(erlangzmq_dealer, {
          identity          :: string(),
          lb                :: list(),
          pending_recv=none :: none | {from, From::term()},
          state=idle        :: idle | wait_req
         }).

valid_peer_type(rep)    -> valid;
valid_peer_type(router) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #erlangzmq_dealer{
               identity=Identity,
               lb=erlangzmq_lb:new()
              },
    {ok, State}.

peer_flags(_State) ->
    {dealer, [incomming_queue]}.

accept_peer(State, PeerPid) ->
    NewLb = erlangzmq_lb:put(State#erlangzmq_dealer.lb, PeerPid),
    {reply, {ok, PeerPid}, State#erlangzmq_dealer{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, _Data, _From) ->
    {reply, {error, not_implemented_yet}, State}.

recv(State, _From) ->
    {reply, {error, not_implemented_yet}, State}.

send_multipart(#erlangzmq_dealer{lb=LB}=State, Multipart, From) ->
    Traffic = erlangzmq_protocol:encode_message_multipart(Multipart),

    case erlangzmq_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            erlangzmq_peer:send(PeerPid, Traffic, From),
            {noreply, State#erlangzmq_dealer{lb=NewLB}}
    end.

recv_multipart(#erlangzmq_dealer{state=idle, lb=LB}=State, From) ->
    case erlangzmq_lb:get(LB) of
        none ->
            {noreply, State#erlangzmq_dealer{state=wait_req, pending_recv={from, From}}};
        {NewLB, PeerPid} ->
            direct_recv_multipart(State#erlangzmq_dealer{lb=NewLB}, PeerPid, PeerPid, From)
    end;
recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use incomming_queue property
    {noreply, State}.

queue_ready(#erlangzmq_dealer{state=wait_req, pending_recv={from, PendingRecv}}=State, _Identity, PeerPid) ->
    case erlangzmq_peer:incomming_queue_out(PeerPid) of
        {out, Messages} ->
            gen_server:reply(PendingRecv, {ok, Messages});
        empty ->
            gen_server:reply(PendingRecv, {error, queue_empty})
    end,

    FutureState = State#erlangzmq_dealer{state=idle, pending_recv=none},
    {noreply, FutureState};

queue_ready(State, _Identity, _PeerPid) ->
    {noreply, State}.

peer_disconected(#erlangzmq_dealer{lb=LB}=State, PeerPid) ->
    NewLB = erlangzmq_lb:delete(LB, PeerPid),
    {noreply, State#erlangzmq_dealer{lb=NewLB}}.

%% implement direct recv from peer queues
direct_recv_multipart(#erlangzmq_dealer{lb=LB}=State, FirstPeerPid, PeerPid, From) ->
    case erlangzmq_peer:incomming_queue_out(PeerPid) of
        {out, Messages} ->
            {reply, {ok, Messages}, State};

        empty ->
            case erlangzmq_lb:get(LB) of
                {NewLB, FirstPeerPid} ->
                    {noreply, State#erlangzmq_dealer{state=wait_req, pending_recv={from, From}, lb=NewLB}};
                {NewLB, OtherPeerPid} ->
                    direct_recv_multipart(State#erlangzmq_dealer{lb=NewLB}, FirstPeerPid, OtherPeerPid, From)
            end
    end.
