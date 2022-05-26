%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Pull Pattern for Erlang
%%
%% This pattern implement Pull especification
%% from: http://rfc.zeromq.org/spec:30/PIPELINE#toc4

-module(chumak_pull).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1
        ]).

-record(chumak_pull, {
          identity               :: string(),
          pending_recv           :: nil | {from, From::term()},
          pending_recv_multipart :: nil | {from, From::term()},
          recv_queue             :: queue:queue()
         }).

valid_peer_type(push)    -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #chumak_pull{
               identity=Identity,
               recv_queue=queue:new(),
               pending_recv=nil,
               pending_recv_multipart=nil
              },
    {ok, State}.

terminate(_Reason, #chumak_pull{pending_recv=Recv, pending_recv_multipart=RecvM}) ->
    case Recv of
        {from, From} -> gen_server:reply(From, {error, closed});
        _ -> ok
    end,

    case RecvM of
        {from, FromM} -> gen_server:reply(FromM, {error, closed});
        _ -> ok
    end,

    ok.

identity(#chumak_pull{identity=Identity}) -> Identity.

peer_flags(_State) ->
    {pull, [incoming_queue]}.

accept_peer(State, PeerPid) ->
    {reply, {ok, PeerPid}, State}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(#chumak_pull{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#chumak_pull.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            Msg = binary:list_to_bin(Multipart),
            {reply, {ok, Msg}, State#chumak_pull{recv_queue=NewRecvQueue}};
        {empty, _RecvQueue} ->
            {noreply, State#chumak_pull{pending_recv={from, From}}}
    end;

recv(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_use}, State}.

recv_multipart(#chumak_pull{pending_recv=nil, pending_recv_multipart=nil}=State, From) ->
    case queue:out(State#chumak_pull.recv_queue) of
        {{value, Multipart}, NewRecvQueue} ->
            {reply, {ok, Multipart}, State#chumak_pull{recv_queue=NewRecvQueue}};

        {empty, _RecvQueue} ->
            {noreply, State#chumak_pull{pending_recv_multipart={from, From}}}
    end;

recv_multipart(State, _From) ->
    {reply, {error, already_pending_recv}, State}.

peer_recv_message(State, _Message, _From) ->
    %% This function will never called, because use incoming_queue property
    {noreply, State}.

unblock(#chumak_pull{pending_recv=Recv,pending_recv_multipart=MultiRecv}=State, _From) ->
    NewState =
        case Recv of
            {from, From} ->
                gen_server:reply(From, {error, again}),
                State#chumak_pull{pending_recv=nil};
            nil -> State
        end,
    MultiNewState =
        case MultiRecv of
            {from, MultiFrom} ->
                gen_server:reply(MultiFrom, {error, again}),
                NewState#chumak_pull{pending_recv_multipart=nil};
            nil -> NewState
        end,
    {reply, ok, MultiNewState}.

queue_ready(State, _Identity, PeerPid) ->
    case chumak_peer:incoming_queue_out(PeerPid) of
        {out, Multipart} ->
            {noreply,handle_queue_ready(State,Multipart)};
        empty ->
            {noreply,State};
        {error,Info}->
            ?LOG_WARNING("zmq queue error", #{error => send_error, reason => Info}),
            {noreply,State}
    end.

peer_disconected(State, _PeerPid) ->
    {noreply, State}.

handle_queue_ready(#chumak_pull{pending_recv=nil, pending_recv_multipart=nil}=State,Data)->
    NewRecvQueue = queue:in(Data, State#chumak_pull.recv_queue),
    State#chumak_pull{recv_queue=NewRecvQueue};

%% when pending recv
handle_queue_ready(#chumak_pull{pending_recv={from, PendingRecv}, pending_recv_multipart=nil}=State, Data)->
    Msg = binary:list_to_bin(Data),
    gen_server:reply(PendingRecv, {ok, Msg}),
    State#chumak_pull{pending_recv=nil};

%% when pending recv_multipart
handle_queue_ready(#chumak_pull{pending_recv=nil, pending_recv_multipart={from, PendingRecv}}=State, Data)->
    gen_server:reply(PendingRecv, {ok, Data}),
    State#chumak_pull{pending_recv_multipart=nil}.
