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

%% @doc ZeroMQ Pattern behaviour for Erlang
%%
%% This behaviour defines all methods that a pattern needs to implement.

-module(erlangzmq_pattern).
-include("erlangzmq.hrl").

-export([module/1, error_msg/1]).

-type pattern_state() :: tuple().
-type module_name() :: erlangzmq_pattern_req.

-callback valid_peer_type(SocketType::socket_type()) -> valid | invalid.
-callback init(Identity::string()) -> {ok, pattern_state()}.
-callback peer_flags(State::pattern_state()) -> {SocketType::socket_type(), [PeerFlag::term()]}.
-callback accept_peer(State::pattern_state(), PeerPid::pid()) -> Reply::term().
-callback peer_ready(State::pattern_state(), PeerPid::pid(), Identity::binary()) -> Reply::term().
-callback send(State::pattern_state(), Data::binary(), From::term()) -> Reply::term().
-callback recv(State::pattern_state(), From::term()) -> Reply::term().
-callback identity(State::pattern_state()) -> Identity::string().

%% Multipart support
-callback send_multipart(State::pattern_state(), [Data::binary()], From::term()) -> Reply::term().
-callback recv_multipart(State::pattern_state(), From::term()) -> Reply::term().

-callback peer_recv_message(State::pattern_state(), Message::erlangzmq_protocol:message(), From::pid()) -> Reply::term().
-callback queue_ready(State::pattern_state(), Identity::string(), From::pid()) -> Reply::term().
-callback peer_disconected(State::pattern_state(), PeerPid::pid()) -> Reply::term().


%% @doc find matching pattern for a socket type.
-spec module(SocketType::socket_type()) -> module_name() | {error, invalid_socket_type}.
module(req)    -> erlangzmq_req;
module(rep)    -> erlangzmq_rep;
module(dealer) -> erlangzmq_dealer;
module(router) -> erlangzmq_router;
module(pub)    -> erlangzmq_pub;
module(sub)    -> erlangzmq_sub;
module(xpub)   -> erlangzmq_xpub;
module(xsub)   -> erlangzmq_xsub;
module(push)   -> erlangzmq_push;
module(pull)   -> erlangzmq_pull;
module(pair)   -> erlangzmq_pair;
module(_)      -> {error, invalid_socket_type}.

%% @doc helper to translate reason atom to human-readable
error_msg(efsm) ->
    "operation cannot be performed on this socket at the moment".
