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

%% @doc ZeroMQ XPub Pattern for Erlang
%%
%% This pattern implement XPub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc4

-module(erlangzmq_xpub).
-behaviour(erlangzmq_pattern).
-define(PUB, erlangzmq_pub).
-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, peer_subscribe/3, peer_cancel_subscribe/3,
         identity/1
        ]).

init(Identity) ->
    ?PUB:init(Identity, [xpub]).

identity(State) -> ?PUB:identity(State).
valid_peer_type(SocketType) -> ?PUB:valid_peer_type(SocketType).
peer_flags(State) -> ?PUB:peer_flags(State).
accept_peer(State, PeerPid) -> ?PUB:accept_peer(State, PeerPid).
peer_ready(State, PeerPid, Identity) -> ?PUB:peer_ready(State, PeerPid, Identity).
send(State, Data, From) -> ?PUB:send(State, Data, From).
recv(State, From) -> ?PUB:recv(State, From).
send_multipart(State, Data, From) -> ?PUB:send_multipart(State, Data, From).
recv_multipart(State, From) -> ?PUB:recv_multipart(State, From).
peer_recv_message(State, Message, From) -> ?PUB:peer_recv_message(State, Message, From).
queue_ready(State, Identity, From) -> ?PUB:queue_ready(State, Identity, From).
peer_disconected(State, PeerPid) -> ?PUB:peer_disconected(State, PeerPid).

%% Pub specific
peer_subscribe(State, PeerPid, Subscription) ->
	?PUB:peer_subscribe(State, PeerPid, Subscription).
peer_cancel_subscribe(State, PeerPid, Subscription) -> 
	?PUB:peer_cancel_subscribe(State,  PeerPid, Subscription).
