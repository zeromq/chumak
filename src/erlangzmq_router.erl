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

%% @doc ZeroMQ Router Pattern for Erlang
%%
%% This pattern implement Router especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc6

-module(erlangzmq_router).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2]).

-record(erlangzmq_router, {
          identity     :: string(),
          lbs,                         %% loadbalancers based on identity
          pending_recv :: nil | pid(),
          recv_queue   :: queue:queue()
}).

valid_peer_type(req)    -> valid;
valid_peer_type(router) -> valid;
valid_peer_type(dealer) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #erlangzmq_router{
               identity=Identity,
               lbs=erlangzmq_lbs:new(),
               recv_queue=queue:new(),
               pending_recv=nil
              },
    {ok, State}.

peer_flags(_State) ->
    {router, [incomming_queue]}.

accept_peer(State, PeerPid) ->
    {reply, {ok, PeerPid}, State}.

peer_ready(#erlangzmq_router{lbs=LBs}=State, PeerPid, Identity) ->
    NewLBs = erlangzmq_lbs:put(LBs, Identity, PeerPid),
    {noreply, State#erlangzmq_router{lbs=NewLBs}}.

send(State, _Data, _From) ->
    {reply, {error, send_implemented_yet}, State}.

recv(State, _From) ->
    {reply, {error, recv_implemented_yet}, State}.

send_multipart(#erlangzmq_router{lbs=LBs}=State, Multipart, _From) when length(Multipart) >= 2 ->
    [Identity|RemmaingMultipart] = Multipart,
    case erlangzmq_lbs:get(LBs, binary_to_list(Identity)) of
        {NewLBs, PeerPid} ->
            Traffic = erlangzmq_protocol:encode_message_multipart(RemmaingMultipart),
            erlangzmq_peer:send(PeerPid, Traffic),
            {reply, ok, State#erlangzmq_router{lbs=NewLBs}};

        none ->
           {reply, {error, no_peers}, State}
    end;

send_multipart(State, _Multipart, _From) ->
    {reply, {error, identity_missing}, State}.

recv_multipart(#erlangzmq_router{recv_queue=RecvQueue, pending_recv=nil}=State, From) ->
    case queue:out(RecvQueue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#erlangzmq_router{recv_queue=NewRecvQueue}};
        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_router{pending_recv=From}}
    end;
recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use incomming_queue property
    {noreply, State}.

queue_ready(#erlangzmq_router{recv_queue=RecvQueue, pending_recv=nil}=State, Identity, PeerPid) ->
    MultiPart = recv_message(Identity, PeerPid),
    NewRecvQueue = queue:in(MultiPart, RecvQueue),
    {noreply, State#erlangzmq_router{recv_queue=NewRecvQueue}};

queue_ready(#erlangzmq_router{pending_recv=PendingRecv}=State, Identity, PeerPid) ->
    MultiPart = recv_message(Identity, PeerPid),
    gen_server:reply(PendingRecv, {ok, MultiPart}),
    {noreply, State#erlangzmq_router{pending_recv=nil}}.

peer_disconected(#erlangzmq_router{lbs=LBs}=State, PeerPid) ->
    NewLBs = erlangzmq_lb:delete(LBs, PeerPid),
    {noreply, State#erlangzmq_router{lbs=NewLBs}}.

recv_message(Identity, PeerPid) ->
    IdentityBin = list_to_binary(Identity),
    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),
    [IdentityBin | Multipart].
