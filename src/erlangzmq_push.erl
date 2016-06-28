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

%% @doc ZeroMQ Push Pattern for Erlang
%%
%% This pattern implement Push especification
%% from: http://rfc.zeromq.org/spec:30/PIPELINE#toc3

-module(erlangzmq_push).
-behaviour(erlangzmq_pattern).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2
        ]).

-record(erlangzmq_push, {
          identity         :: string(),
          lb               :: list()
         }).

valid_peer_type(pull)    -> valid;
valid_peer_type(_)      -> invalid.

init(Identity) ->
    State = #erlangzmq_push{
               identity=Identity,
               lb=erlangzmq_lb:new()
              },
    {ok, State}.

peer_flags(_State) ->
    {push, []}.

accept_peer(State, PeerPid) ->
    NewLb = erlangzmq_lb:put(State#erlangzmq_push.lb, PeerPid),
    {reply, {ok, PeerPid}, State#erlangzmq_push{lb=NewLb}}.

peer_ready(State, _PeerPid, _Identity) ->
    {noreply, State}.

send(State, Data, From) ->
    send_multipart(State, [Data], From).

recv(State, From) ->
    recv_multipart(State, From).

send_multipart(#erlangzmq_push{lb=LB}=State, Multipart, From) ->
    Traffic = erlangzmq_protocol:encode_message_multipart(Multipart),

    case erlangzmq_lb:get(LB) of
        none ->
            {reply, {error, no_connected_peers}, State};
        {NewLB, PeerPid} ->
            erlangzmq_peer:send(PeerPid, Traffic, From),
            {noreply, State#erlangzmq_push{lb=NewLB}}
    end.

recv_multipart(State, _From) ->
    {reply, {error, not_use}, State}.

peer_recv_message(State, _Message, _From) ->
     %% This function will never called, because use PUSH not receive messages
    {noreply, State}.

queue_ready(State, _Identity, _PeerPid) ->
     %% This function will never called, because use PUB not receive messages
    {noreply, State}.

peer_disconected(#erlangzmq_push{lb=LB}=State, PeerPid) ->
    NewLB = erlangzmq_lb:delete(LB, PeerPid),
    {noreply, State#erlangzmq_push{lb=NewLB}}.
