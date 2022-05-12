%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Rep Pattern for Erlang
%%
%% This pattern implement REP especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc4

-module(chumak_rep).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1]).

%% state for a pattern always to be module name.
-record(chumak_rep, {
          identity           :: string(),
          pending_recv=nil   :: nil | {from, From::term()},
          state=idle         :: idle | wait_req,
          lb                 :: list(),
          last_recv_peer=nil :: nil | pid()
}).

valid_peer_type(req)    -> valid;
valid_peer_type(dealer) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #chumak_rep{
               identity=Identity,
               lb=chumak_lb:new()
              },
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

identity(#chumak_rep{identity=I}) -> I.

peer_flags(_State) ->
    {rep, [incoming_queue]}.

accept_peer(State, PeerPid) ->
    NewLb = chumak_lb:put(State#chumak_rep.lb, PeerPid),
    {reply, {ok, PeerPid}, State#chumak_rep{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(#chumak_rep{last_recv_peer=nil}=State, _Data, _From) ->
    {reply, {error, efsm}, State};

send(#chumak_rep{last_recv_peer=LastRecvPeer}=State, Data, _From)
  when is_pid(LastRecvPeer) ->
    chumak_peer:send(LastRecvPeer, [<<>>, Data]),
    {reply, ok, State#chumak_rep{last_recv_peer=nil}}.


recv(#chumak_rep{state=idle, lb=LB}=State, From) ->
    case chumak_lb:get(LB) of
        none ->
            {noreply, State#chumak_rep{state=wait_req, pending_recv={from, From}}};
        {NewLB, PeerPid} ->
            direct_recv(State#chumak_rep{lb=NewLB}, PeerPid, PeerPid, From)
    end;

recv(State, _From) ->
    {reply, {error, efsm}, State}.

unblock(#chumak_rep{pending_recv={from, PendingRecv}}=State, _From) ->
    NewState = State#chumak_rep{pending_recv=nil},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState};

unblock(#chumak_rep{pending_recv=nil}=State, _From) ->
    {reply, ok, State}.


send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_implemented_yet}, State}.

recv_multipart(State, _From) ->
    {reply, {error, not_implemented_yet}, State}.

peer_recv_message(State, _Message, _From) ->
    %% This function will never called, because use incoming_queue property
    {noreply, State}.

queue_ready(#chumak_rep{state=wait_req, pending_recv={from, PendingRecv}}=State, _Identity, PeerPid) ->
    FutureState = State#chumak_rep{state=idle, pending_recv=nil},
    case recv_from_peer(PeerPid) of
        {ok, Message} ->
            gen_server:reply(PendingRecv, {ok, Message}),
            {noreply, FutureState#chumak_rep{last_recv_peer=PeerPid}};

        {error, Reason} ->
            gen_server:reply(PendingRecv, {error, Reason}),
            {noreply, FutureState};

        empty ->
            gen_server:reply(PendingRecv, {error, queue_empty}),
            {noreply, FutureState}
    end;

queue_ready(State, _Identity, _PeerPid) ->
    %% Not used in iddle state
    {noreply, State}.

peer_disconected(#chumak_rep{lb=LB}=State, PeerPid) ->
    NewLB = chumak_lb:delete(LB, PeerPid),
    {noreply, State#chumak_rep{lb=NewLB}}.

%% implement direct recv from peer queues
direct_recv(#chumak_rep{lb=LB}=State, FirstPeerPid, PeerPid, From) ->
    case recv_from_peer(PeerPid) of
        {ok, Message} ->
            {reply, {ok, Message}, State#chumak_rep{last_recv_peer=PeerPid}};

        {error, Reason} ->
            {reply, {error, Reason}, State};

        empty ->
            case chumak_lb:get(LB) of
                {NewLB, FirstPeerPid} ->
                    {noreply, State#chumak_rep{state=wait_req, pending_recv={from, From}, lb=NewLB}};
                {NewLB, OtherPeerPid} ->
                    direct_recv(State#chumak_rep{lb=NewLB}, FirstPeerPid, OtherPeerPid, From)
            end
    end.

recv_from_peer(PeerPid) ->
    case chumak_peer:incoming_queue_out(PeerPid) of
        {out, Messages} ->
            decode_messages(Messages);
        empty ->
            empty;
        {error,Info}->
            ?LOG_WARNING("zmq send error", #{error => send_error, reason => Info}),
            empty
    end.

decode_messages([<<>>|Tail])->
    {ok, binary:list_to_bin(Tail)};
decode_messages([Delimiter|_Tail]) ->
    ?LOG_WARNING("zmq decode error", #{error => invalid_delimiter_frame, pattern => rep, obtained_frame => Delimiter, expected_frame => <<>> }),
    {error, invalid_delimiter_frame}.
