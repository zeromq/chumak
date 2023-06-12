%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Req Pattern for Erlang
%%
%% This pattern implement REQ especification
%% from: http://rfc.zeromq.org/spec:28/REQREP#toc3

-module(chumak_req).
-behaviour(chumak_pattern).
-include_lib("kernel/include/logger.hrl").

-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, identity/1]).

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
-record(chumak_req, {
          identity           :: string(),
          lb                 :: list(),
          state=ready        :: req_state(),
          last_peer_sent=nil :: nil | pid(),
          pending_recv=nil   :: term(),
          msg_buf=[]         :: list(),
          is_multipart=false :: boolean()
}).

valid_peer_type(rep)    -> valid;
valid_peer_type(router) -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #chumak_req{
               identity=Identity,
               lb=chumak_lb:new(),
               msg_buf=[]
              },
    {ok, State}.

terminate(_Reason, #chumak_req{pending_recv=nil}) ->
    ok;
terminate(_Reason, #chumak_req{pending_recv=From}) ->
    gen_server:reply(From, {error, closed}),
    ok.

identity(#chumak_req{identity=I}) -> I.

peer_flags(_State) ->
    {req, []}.

accept_peer(State, PeerPid) ->
    NewLb = chumak_lb:put(State#chumak_req.lb, PeerPid),
    {reply, {ok, PeerPid}, State#chumak_req{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(#chumak_req{lb=LB, state=ready}=State, Data, From) ->
    case chumak_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            chumak_peer:send(PeerPid, [<<>>, Data], From),
            {noreply, State#chumak_req{
                        lb=NewLB,
                        state=wait_reply,
                        last_peer_sent=PeerPid
                       }}
    end;

send(State, _Data, _From) ->
    {reply, {error, efsm}, State}.

%% send recv when is already waiting recv command
recv(#chumak_req{state=wait_recv, msg_buf=Buffer}=State, _From) ->
    FullMsg = binary:list_to_bin(Buffer),
    {reply, {ok, FullMsg}, State#chumak_req{state=ready, msg_buf=[]}};

%% not allow recv in 'ready' state
recv(#chumak_req{state=ready}=State, _From) ->
    {reply, {error, efsm}, State};

%% when in other state wait_reply and wait_more_msg
recv(#chumak_req{state=SocketState, pending_recv=PendingRecv}=State, From) ->
    case {SocketState, PendingRecv} of
        {wait_reply, nil} ->
            {noreply, State#chumak_req{pending_recv=From, is_multipart=false}};

        {wait_more_msg, nil} ->
            {noreply, State#chumak_req{pending_recv=From, is_multipart=false}};

        _ ->
            {reply, {error, efsm}, State}
    end.

send_multipart(#chumak_req{lb=LB, state=ready}=State, Multipart, From) ->
    case chumak_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            chumak_peer:send(PeerPid, [<<>>|Multipart], From),
            {noreply, State#chumak_req{
                        lb=NewLB,
                        state=wait_reply,
                        last_peer_sent=PeerPid
                       }}
    end;
send_multipart(State, _Multipart, _From) ->
    {reply, {error, efsm}, State}.

recv_multipart(#chumak_req{state=wait_recv, msg_buf=Buffer}=State, _From) ->
    {reply, {ok, Buffer}, State#chumak_req{state=ready, msg_buf=[]}};
recv_multipart(#chumak_req{state=wait_reply, pending_recv=nil}=State, From) ->
    {noreply, State#chumak_req{pending_recv=From, is_multipart=true}};
recv_multipart(#chumak_req{state=wait_more_msg, pending_recv=nil}=State, From) ->
    {noreply, State#chumak_req{pending_recv=From, is_multipart=true}};
recv_multipart(State, _From) ->
    {reply, {error, efsm}, State}.

unblock(#chumak_req{pending_recv=nil}=State, _From) ->
    {reply, ok, State};
unblock(#chumak_req{pending_recv=PendingRecv}=State, _From) ->
    NewState = State#chumak_req{pending_recv=nil},
    gen_server:reply(PendingRecv, {error, again}),
    {reply, ok, NewState}.

peer_recv_message(#chumak_req{state=wait_reply, last_peer_sent=From}=State, Message, From) ->
    case chumak_protocol:message_data(Message) of
        <<>> ->
            {noreply, State#chumak_req{state=wait_more_msg}};
        Frame ->
            ?LOG_WARNING("zmq recieve error", #{error => invalid_delimiter_frame, pattern => req, obtained_frame => Frame, expected_frame => <<>>}),
            {noreply, State}
    end;

peer_recv_message(#chumak_req{state=wait_more_msg, last_peer_sent=From}=State, Message, From) ->
    #chumak_req{msg_buf=Buffer, pending_recv=PendingRecv, is_multipart=IsMultipart} = State,
    NewBuffer = Buffer ++ [chumak_protocol:message_data(Message)],

    case {chumak_protocol:message_has_more(Message), PendingRecv} of
        %% if need to accumulate more message
        {true, _} ->
            {noreply, State#chumak_req{state=wait_more_msg, msg_buf=NewBuffer}};

        %% the last message was received, but the client not called recv yet
        {false, nil} ->
            {noreply, State#chumak_req{state=wait_recv, msg_buf=NewBuffer}};

        %% client already called recv
        {false, PendingRecv} ->
            Reply = case IsMultipart of
                false -> binary:list_to_bin(NewBuffer);
                true -> NewBuffer
            end,
            gen_server:reply(PendingRecv, {ok, Reply}),
            {noreply, State#chumak_req{state=ready, msg_buf=[], pending_recv=nil}}
    end;

peer_recv_message(#chumak_req{last_peer_sent=LastPeer, state=S}=State, Message, From) ->
    ?LOG_WARNING("zmq receive error", #{error => discard_message, last_peer_sent => LastPeer, peer_sent => From, state => S, message => Message}),
    {noreply, State}.

queue_ready(State, _Identity, _From) ->
    %% Not used in REQ Pattern
    {noreply, State}.

peer_disconected(#chumak_req{lb=LB}=State, PeerPid) ->
    NewLB = chumak_lb:delete(LB, PeerPid),
    {noreply, State#chumak_req{lb=NewLB}}.
