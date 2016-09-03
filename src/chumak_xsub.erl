%% @copyright 2016 Choven Corp.
%%
%% This file is part of chumak.
%%
%% chumak is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% chumak is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with chumak.  If not, see <http://www.gnu.org/licenses/>

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
