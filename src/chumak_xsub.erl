%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ XSub Pattern for Erlang
%%
%% This pattern implement XSub especification
%% from: http://rfc.zeromq.org/spec:29/PUBSUB#toc6

-module(chumak_xsub).
-define (SUB, chumak_sub).

-export([valid_peer_type/1, init/1, peer_flags/1, accept_peer/2, peer_ready/3,
         send/3, recv/2,
         send_multipart/3, recv_multipart/2, peer_recv_message/3,
         queue_ready/3, peer_disconected/2, subscribe/2, cancel/2,
         peer_reconnected/2, identity/1
        ]).

init(Identity) ->
    ?SUB:init(Identity, [xsub]).

identity(State) -> ?SUB:identity(State).
valid_peer_type(SocketType) -> ?SUB:valid_peer_type(SocketType).
peer_flags(State) -> ?SUB:peer_flags(State).
accept_peer(State, PeerPid) -> ?SUB:accept_peer(State, PeerPid).
peer_ready(State, PeerPid, Identity) -> ?SUB:peer_ready(State, PeerPid, Identity).
send(State, Data, From) -> ?SUB:send(State, Data, From).
recv(State, From) -> ?SUB:recv(State, From).
send_multipart(State, Data, From) -> ?SUB:send_multipart(State, Data, From).
recv_multipart(State, From) -> ?SUB:recv_multipart(State, From).
peer_recv_message(State, Message, From) -> ?SUB:peer_recv_message(State, Message, From).
queue_ready(State, Identity, From) -> ?SUB:queue_ready(State, Identity, From).
peer_disconected(State, PeerPid) -> ?SUB:peer_disconected(State, PeerPid).

%% Other sub-specific
cancel(State, Topic) -> ?SUB:cancel(State, Topic).
peer_reconnected(State, PeerPid) -> ?SUB:peer_reconnected(State, PeerPid).
subscribe(State, Topic) -> ?SUB:subscribe(State, Topic).
