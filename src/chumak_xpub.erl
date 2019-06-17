%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ XPub Pattern for Erlang
%%
%% This pattern implement XPub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc4

-module(chumak_xpub).
-behaviour(chumak_pattern).
-define(PUB, chumak_pub).
-export([valid_peer_type/1, init/1, terminate/2, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         unblock/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, peer_subscribe/3, peer_cancel_subscribe/3,
         identity/1
        ]).

init(Identity) ->
    ?PUB:init(Identity, [xpub]).

terminate(Reason, State) ->
    ?PUB:terminate(Reason, State).

identity(State) -> ?PUB:identity(State).
valid_peer_type(SocketType) -> ?PUB:valid_peer_type(SocketType).
peer_flags(State) -> ?PUB:peer_flags(State).
accept_peer(State, PeerPid) -> ?PUB:accept_peer(State, PeerPid).
peer_ready(State, PeerPid, Identity) -> ?PUB:peer_ready(State, PeerPid, Identity).
send(State, Data, From) -> ?PUB:send(State, Data, From).
recv(State, From) -> ?PUB:recv(State, From).
send_multipart(State, Data, From) -> ?PUB:send_multipart(State, Data, From).
recv_multipart(State, From) -> ?PUB:recv_multipart(State, From).
unblock(State, From) -> ?PUB:unblock(State, From).
peer_recv_message(State, Message, From) -> ?PUB:peer_recv_message(State, Message, From).
queue_ready(State, Identity, From) -> ?PUB:queue_ready(State, Identity, From).
peer_disconected(State, PeerPid) -> ?PUB:peer_disconected(State, PeerPid).

%% Pub specific
peer_subscribe(State, PeerPid, Subscription) ->
	?PUB:peer_subscribe(State, PeerPid, Subscription).
peer_cancel_subscribe(State, PeerPid, Subscription) -> 
	?PUB:peer_cancel_subscribe(State,  PeerPid, Subscription).
