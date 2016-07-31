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

%% @doc ZeroMQ Pair Pattern for Erlang
%%
%% This pattern implement Pair especification
%% from: http://rfc.zeromq.org/spec:31/EXPAIR#toc3

-module(erlangzmq_pair).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1
        ]).

-record(erlangzmq_pair, {
          identity               :: string(),
          pair_pid               :: nil | pid(),
          pending_send           :: nil | {term(), binary()},
          pending_recv           :: nil | term(),
          pending_recv_multipart :: nil | term(),
          recv_queue             :: queue:queue()
         }).

valid_peer_type(pair) -> valid;
valid_peer_type(_)    -> invalid.

init(Identity) ->
    State = #erlangzmq_pair{
               identity=Identity,
               pair_pid=nil,
               pending_recv=nil,
               pending_recv_multipart=nil,
               pending_send=nil,
               recv_queue=queue:new()
              },
    {ok, State}.

identity(#erlangzmq_pair{identity=Identity}) -> Identity.

peer_flags(_State) ->
    {pair, [incomming_queue]}.

accept_peer(#erlangzmq_pair{pair_pid=nil}=State, PeerPid) ->
    {reply, {ok, PeerPid}, State#erlangzmq_pair{pair_pid=PeerPid}};

accept_peer(State, PeerPid) ->
    error_logger:info_msg("Deny remote peer, this peer already paired"),
    erlangzmq_peer:send_error(PeerPid, "This peer is already paired"),
    erlangzmq_peer:close(PeerPid),
    {reply, {error, peer_already_paired}, State}.

peer_ready(#erlangzmq_pair{pending_send=PendingSend, pair_pid=PeerPid}=State, PeerPid, _Identity) ->
    case PendingSend of
        {From, Traffic} ->
            erlangzmq_peer:send(PeerPid, Traffic, From);
        nil ->
            pass
    end,
    {noreply, State#erlangzmq_pair{pending_send=nil}};

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#erlangzmq_pair{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#erlangzmq_pair.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            Msg = binary:list_to_bin(Multipart),
            {reply, {ok, Msg}, State#erlangzmq_pair{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_pair{pending_recv=From}}
    end;

recv(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

send_multipart(#erlangzmq_pair{pending_send=nil, pair_pid=nil}=State, Multipart, From) ->
    %% set send await
    Traffic = erlangzmq_protocol:encode_message_multipart(Multipart),
    {noreply, State#erlangzmq_pair{pending_send={From, Traffic}}};

send_multipart(#erlangzmq_pair{pending_send=nil, pair_pid=PeerPid}=State, Multipart, From) ->
    %% send messsage now
    Traffic = erlangzmq_protocol:encode_message_multipart(Multipart),
    erlangzmq_peer:send(PeerPid, Traffic, From),
    {noreply, State};

send_multipart(State, _Multipart, _From) ->
    {reply, {error, pendind_send_already_called}, State}.

recv_multipart(#erlangzmq_pair{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#erlangzmq_pair.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#erlangzmq_pair{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#erlangzmq_pair{pending_recv_multipart=From}}
    end;

recv_multipart(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PAIR use the incomming_queue parameter
    {noreply, State}.

queue_ready(
  #erlangzmq_pair{pair_pid=PeerPid,
                  pending_recv=PendingRecv,
                  pending_recv_multipart=PendingRecvMultiPart,
                  recv_queue=RecvQueue}=State, _Identity, PeerPid) ->

    {out, Multipart} = erlangzmq_peer:incomming_queue_out(PeerPid),

    NewRecvQueue = case {PendingRecv, PendingRecvMultiPart} of
                       {nil, nil} ->
                           queue:in(Multipart, RecvQueue);

                       {_, nil} ->
                           Msg = binary:list_to_bin(Multipart),
                           gen_server:reply(PendingRecv, {ok, Msg}),
                           RecvQueue;

                       {nil, _}->
                           gen_server:reply(PendingRecvMultiPart, {ok, Multipart}),
                           RecvQueue
                   end,

    {noreply, State#erlangzmq_pair{pending_recv=nil, pending_recv_multipart=nil, recv_queue=NewRecvQueue}};

queue_ready(State, _Identity, _PeerPid) ->
    {noreply, State}.

peer_disconected(State, _PeerPid) ->
    {noreply, State#erlangzmq_pair{pair_pid=nil}}.
