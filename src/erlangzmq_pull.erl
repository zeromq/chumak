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

%% @doc ZeroMQ Pull Pattern for Erlang
%%
%% This pattern implement Pull especification
%% from: http://rfc.zeromq.org/spec:30/PIPELINE#toc4

-module(erlangzmq_pull).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2
        ]).

-record(erlangzmq_pull, {
          identity               :: string(),
          pending_recv           :: nil | {from, From::term()},
          pending_recv_multipart :: nil | {from, From::term()},
          recv_queue             :: queue:queue()
         }).

valid_peer_type(push)    -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #erlangzmq_pull{
               identity=Identity,
               recv_queue=queue:new(),
               pending_recv=nil,
               pending_recv_multipart=nil
              },
    {ok, State}.

peer_flags(_State) ->
    {pull, [incomming_queue]}.

accept_peer(State, PeerPid) ->
    {reply, {ok, PeerPid}, State}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#erlangzmq_pull{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#erlangzmq_pull.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            Msg = binary:list_to_bin(Multipart),
            {reply, {ok, Msg}, State#erlangzmq_pull{recv_queue=NewRecvQueue}};
        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_pull{pending_recv={from, From}}}
    end;

recv(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_use}, State}.

recv_multipart(#erlangzmq_pull{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#erlangzmq_pull.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#erlangzmq_pull{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_pull{pending_recv_multipart={from, From}}}
    end;

recv_multipart(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

peer_recv_message(State, _Message, _From) ->
    %% This function will never called, because use incomming_queue property
    {noreply, State}.

queue_ready(#erlangzmq_pull{pending_recv=nil, pending_recv_multipart=nil}=State, _Identity, PeerPid) ->
    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),
    NewRecvQueue = queue:in(Multipart, State#erlangzmq_pull.recv_queue),
    {noreply, State#erlangzmq_pull{recv_queue=NewRecvQueue}};

%% when pending recv
queue_ready(#erlangzmq_pull{pending_recv={from, PendingRecv}, pending_recv_multipart=nil}=State, _Identity, PeerPid) ->
    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),
    Msg = binary:list_to_bin(Multipart),
    gen_server:reply(PendingRecv, {ok, Msg}),
    {noreply, State#erlangzmq_pull{pending_recv=nil}};

%% when pending recv_multipart
queue_ready(#erlangzmq_pull{pending_recv=nil, pending_recv_multipart={from, PendingRecv}}=State, _Identity, PeerPid) ->
    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),
    gen_server:reply(PendingRecv, {ok, Multipart}),
    {noreply, State#erlangzmq_pull{pending_recv_multipart=nil}}.

peer_disconected(State, _PeerPid) ->
    {noreply, State}.
