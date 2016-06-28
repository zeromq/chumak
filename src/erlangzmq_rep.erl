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

%% @doc ZeroMQ Rep Pattern for Erlang
%%
%% This pattern implement REP especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc4

-module(erlangzmq_rep).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2]).

%% state for a pattern always to be module name.
-record(erlangzmq_rep, {
          identity           :: string(),
          pending_recv=nil   :: nil | atom(),
          state=idle         :: idle | wait_req,
          lb                 :: list(),
          last_recv_peer=nil :: nil | pid()
}).

valid_peer_type(req)    -> valid;
valid_peer_type(dealer) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #erlangzmq_rep{
               identity=Identity,
               lb=erlangzmq_lb:new()
              },
    {ok, State}.

peer_flags(_State) ->
    {rep, [incomming_queue]}.

accept_peer(State, PeerPid) ->
    NewLb = erlangzmq_lb:put(State#erlangzmq_rep.lb, PeerPid),
    {reply, {ok, PeerPid}, State#erlangzmq_rep{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(#erlangzmq_rep{last_recv_peer=nil}=State, _Data, _From) ->
    {reply, {error, efsm}, State};

send(#erlangzmq_rep{last_recv_peer=LastRecvPeer}=State, Data, _From)
  when is_pid(LastRecvPeer) ->
    Traffic = erlangzmq_protocol:encode_message_multipart([<<>>, Data]),
    erlangzmq_peer:send(LastRecvPeer, Traffic),
    {reply, ok, State#erlangzmq_rep{last_recv_peer=nil}}.


recv(#erlangzmq_rep{state=idle, lb=LB}=State, From) ->
    case erlangzmq_lb:get(LB) of
        none ->
            {noreply, State#erlangzmq_rep{state=wait_req, pending_recv=From}};
        {NewLB, PeerPid} ->
            direct_recv(State#erlangzmq_rep{lb=NewLB}, PeerPid, PeerPid, From)
    end;

recv(State, _From) ->
    {reply, {error, efsm}, State}.

send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_implemented_yet}, State}.

recv_multipart(State, _From) ->
    {reply, {error, not_implemented_yet}, State}.

peer_recv_message(State, _Message, _From) ->
    %% This function will never called, because use incomming_queue property
    {noreply, State}.

queue_ready(#erlangzmq_rep{state=wait_req, pending_recv=PendingRecv}=State, _Identity, PeerPid) ->
    FutureState = State#erlangzmq_rep{state=idle, pending_recv=nil},
    case recv_from_peer(PeerPid) of
        {ok, Message} ->
            gen_server:reply(PendingRecv, {ok, Message}),
            {noreply, FutureState#erlangzmq_rep{last_recv_peer=PeerPid}};

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

peer_disconected(#erlangzmq_rep{lb=LB}=State, PeerPid) ->
    NewLB = erlangzmq_lb:delete(LB, PeerPid),
    {noreply, State#erlangzmq_rep{lb=NewLB}}.

%% implement direct recv from peer queues
direct_recv(#erlangzmq_rep{lb=LB}=State, FirstPeerPid, PeerPid, From) ->
    case recv_from_peer(PeerPid) of
        {ok, Message} ->
            {reply, {ok, Message}, State#erlangzmq_rep{last_recv_peer=PeerPid}};

        {error, Reason} ->
            {reply, {error, Reason}, State};

        empty ->
            case erlangzmq_lb:get(LB) of
                {NewLB, FirstPeerPid} ->
                    {noreply, State#erlangzmq_rep{state=wait_req, pending_recv=From, lb=NewLB}};
                {NewLB, OtherPeerPid} ->
                    direct_recv(State#erlangzmq_rep{lb=NewLB}, FirstPeerPid, OtherPeerPid, From)
            end
    end.

recv_from_peer(PeerPid) ->
    case erlangzmq_peer:incomming_queue_out(PeerPid) of
        {out, Messages} ->
            decode_messages(Messages);
        empty ->
            empty
    end.

decode_messages([<<>>|Tail])->
    {ok, binary:list_to_bin(Tail)};
decode_messages([Delimiter|_Tail]) ->
    error_logger:warning_report({
                                  invalid_delimiter_frame,
                                  {pattern, rep},
                                  {obtained_frame, Delimiter},
                                  {expected_frame, <<>>}
                                }),
    {error, invalid_delimiter_frame}.
