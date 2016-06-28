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

%% @doc ZeroMQ Req Pattern for Erlang
%%
%% This pattern implement REQ especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc3

-module(erlangzmq_req).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2]).

%% The state of REQ pattern, lock-step fsm.
%% +----------+    +---------------+    +------------------+    +-----------------------+    +----------+
%% | s1-ready | => | s2-wait_reply | => | s3-wait_more_msg | => | s4-wait_recv/s1-ready | => | s1-ready |
%% +----------+ e1 +---------------+ e2 +------------------+ e3 +-----------------------+ e4 +----------+
%%
%% Flow of states and events
%% state 1 - ready, the socket not received any send command yet
%%   event 1 - when call send
%% state 2 - waiting a reply, now is allowed to call recv command
%%   event 2 - then socket receive the delimiter message from the last peer that have sent the message.
%% state 3 - waiting more messages
%%   event 3 - receive the last message, if recv is called return to state 1 or else goint to state 4
%% state 4 - the socket is waiting client to call recv method
%%   event 4 - recv method called and the socket will return to state 1.
-type req_state() :: ready | wait_reply | wait_more_msg | wait_recv.

%% state for a pattern always to be module name.
-record(erlangzmq_req, {
          identity         :: string(),
          lb               :: list(),
          state=ready      :: req_state(),
          last_peer_sent   :: pid(),
          pending_recv=nil :: term(),
          msg_buf=[]       :: list()
}).

valid_peer_type(rep)    -> valid;
valid_peer_type(router) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #erlangzmq_req{
               identity=Identity,
               lb=erlangzmq_lb:new(),
               msg_buf=[]
              },
    {ok, State}.

peer_flags(_State) ->
    {req, []}.

accept_peer(State, PeerPid) ->
    NewLb = erlangzmq_lb:put(State#erlangzmq_req.lb, PeerPid),
    {reply, {ok, PeerPid}, State#erlangzmq_req{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(#erlangzmq_req{lb=LB, state=ready}=State, Data, From) ->
    Traffic = erlangzmq_protocol:encode_message_multipart([<<>>, Data]),

    case erlangzmq_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            erlangzmq_peer:send(PeerPid, Traffic, From),
            {noreply, State#erlangzmq_req{
                        lb=NewLB,
                        state=wait_reply,
                        last_peer_sent=PeerPid
                       }}
    end;

send(State, _Data, _From) ->
    {reply, {error, efsm}, State}.

%% send recv when is already waiting recv command
recv(#erlangzmq_req{state=wait_recv, msg_buf=Buffer}=State, _From) ->
    FullMsg = binary:list_to_bin(Buffer),
    {reply, {ok, FullMsg}, State#erlangzmq_req{state=ready, msg_buf=[]}};

%% not allow recv in 'ready' state
recv(#erlangzmq_req{state=ready}=State, _From) ->
    {reply, {error, efsm}, State};

%% when in other state wait_reply and wait_more_msg
recv(#erlangzmq_req{state=SocketState, pending_recv=PendingRecv}=State, From) ->
    case {SocketState, PendingRecv} of
        {wait_reply, nil} ->
            {noreply, State#erlangzmq_req{pending_recv=From}};

        {wait_more_msg, nil} ->
            {noreply, State#erlangzmq_req{pending_recv=From}};

        _ ->
            {reply, {error, efsm}, State}
    end.

send_multipart(State, _Multipart, _From) ->
    {reply, {error, not_implemented_yet}, State}.

recv_multipart(State, _From) ->
    {reply, {error, not_implemented_yet}, State}.


peer_recv_message(#erlangzmq_req{state=wait_reply, last_peer_sent=From}=State, Message, From) ->
    case erlangzmq_protocol:message_data(Message) of
        <<>> ->
            {noreply, State#erlangzmq_req{state=wait_more_msg}};
        Frame ->
            error_logger:warning_report({
                                          invalid_delimiter_frame,
                                          {pattern, req},
                                          {obtained_frame, Frame},
                                          {expected_frame, <<>>}
                                        }),
            {noreply, State}
    end;

peer_recv_message(#erlangzmq_req{state=wait_more_msg, last_peer_sent=From}=State, Message, From) ->
    #erlangzmq_req{msg_buf=Buffer, pending_recv=PendingRecv} = State,
    NewBuffer = Buffer ++ [erlangzmq_protocol:message_data(Message)],

    case {erlangzmq_protocol:message_has_more(Message), PendingRecv} of
        %% if need to accumulate more message
        {true, _} ->
            {noreply, State#erlangzmq_req{state=wait_more_msg, msg_buf=NewBuffer}};

        %% the last message was received, but the client not called recv yet
        {false, nil} ->
            {noreply, State#erlangzmq_req{state=wait_recv, msg_buf=NewBuffer}};

        %% client already called recv
        {false, PendingRecv} ->
            FullMsg = binary:list_to_bin(NewBuffer),
            gen_server:reply(PendingRecv, {ok, FullMsg}),
            {noreply, State#erlangzmq_req{state=ready, msg_buf=[], pending_recv=nil}}
    end;

peer_recv_message(#erlangzmq_req{last_peer_sent=LastPeer, state=S}=State, Message, From) ->
    error_logger:info_report({
                               discard_message,
                               {last_peer_sent, LastPeer},
                               {peer_sent, From},
                               {state, S},
                               {message, Message}
                             }),
    {noreply, State}.

queue_ready(State, _Identity, _From) ->
    %% Not used in REQ Pattern
    {noreply, State}.

peer_disconected(#erlangzmq_req{lb=LB}=State, PeerPid) ->
    NewLB = erlangzmq_lb:delete(LB, PeerPid),
    {noreply, State#erlangzmq_req{lb=NewLB}}.
