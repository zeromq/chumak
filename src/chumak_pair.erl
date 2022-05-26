%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Pair Pattern for Erlang
%%
%% This pattern implement Pair especification
%% from: http://rfc.zeromq.org/spec:31/EXPAIR#toc3

-module(chumak_pair).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1
        ]).

-record(chumak_pair, {
          identity               :: string(),
          pair_pid               :: nil | pid(),
          pending_send           :: nil | {term(), [binary()]},
          pending_recv           :: nil | term(),
          pending_recv_multipart :: nil | term(),
          recv_queue             :: queue:queue()
         }).

valid_peer_type(pair) -> valid;
valid_peer_type(_)    -> invalid.

init(Identity) ->
    State = #chumak_pair{
               identity=Identity,
               pair_pid=nil,
               pending_recv=nil,
               pending_recv_multipart=nil,
               pending_send=nil,
               recv_queue=queue:new()
              },
    {ok, State}.

terminate(_Reason, #chumak_pair{
                      pending_recv=Recv,
                      pending_recv_multipart=RecvM}) ->

    case Recv of
        {from, From} -> gen_server:reply(From, {error, closed});
        _ -> ok
    end,

    case RecvM of
        nil -> ok;
        FromM -> gen_server:reply(FromM, {error, closed})
    end,

    ok.

identity(#chumak_pair{identity=Identity}) -> Identity.

peer_flags(_State) ->
    {pair, [incoming_queue]}.

accept_peer(#chumak_pair{pair_pid=nil}=State, PeerPid) ->
    {reply, {ok, PeerPid}, State#chumak_pair{pair_pid=PeerPid}};

accept_peer(State, PeerPid) ->
    ?LOG_WARNING("zmq connect deny", #{error => already_paired}),
    chumak_peer:send_error(PeerPid, "This peer is already paired"),
    chumak_peer:close(PeerPid),
    {reply, {error, peer_already_paired}, State}.

peer_ready(#chumak_pair{pending_send=PendingSend, pair_pid=PeerPid}=State, PeerPid, _Identity) ->
    case PendingSend of
        {From, Multipart} ->
            chumak_peer:send(PeerPid, Multipart, From);
        nil ->
            pass
    end,
    {noreply, State#chumak_pair{pending_send=nil}};

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#chumak_pair{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#chumak_pair.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            Msg = binary:list_to_bin(Multipart),
            {reply, {ok, Msg}, State#chumak_pair{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#chumak_pair{pending_recv=From}}
    end;

recv(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

unblock(#chumak_pair{pending_recv=PendingRecv}=State, _From) when PendingRecv /= nil ->
    NewState = State#chumak_pair{pending_recv=nil},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState};

unblock(#chumak_pair{pending_recv=nil}=State, _From) ->
    {reply, ok, State}.


send_multipart(#chumak_pair{pending_send=nil, pair_pid=nil}=State, Multipart, From) ->
    %% set send await
    {noreply, State#chumak_pair{pending_send={From, Multipart}}};

send_multipart(#chumak_pair{pending_send=nil, pair_pid=PeerPid}=State, Multipart, From) ->
    %% send message now
    chumak_peer:send(PeerPid, Multipart, From),
    {noreply, State};

send_multipart(State, _Multipart, _From) ->
    {reply, {error, pendind_send_already_called}, State}.

recv_multipart(#chumak_pair{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#chumak_pair.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#chumak_pair{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#chumak_pair{pending_recv_multipart=From}}
    end;

recv_multipart(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PAIR use the incoming_queue parameter
    {noreply, State}.

queue_ready(
  #chumak_pair{pair_pid=PeerPid,
                  pending_recv=PendingRecv,
                  pending_recv_multipart=PendingRecvMultiPart,
                  recv_queue=RecvQueue}=State, _Identity, PeerPid) ->

    {out, Multipart} = chumak_peer:incoming_queue_out(PeerPid),

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

    {noreply, State#chumak_pair{pending_recv=nil, pending_recv_multipart=nil, recv_queue=NewRecvQueue}};

queue_ready(State, _Identity, _PeerPid) ->
    {noreply, State}.

peer_disconected(State, _PeerPid) ->
    {noreply, State#chumak_pair{pair_pid=nil}}.
