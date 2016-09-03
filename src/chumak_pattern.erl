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

%% @doc ZeroMQ Pattern behaviour for Erlang
%%
%% This behaviour defines all methods that a pattern needs to implement.

-module(chumak_pattern).
-include("chumak.hrl").

-export([module/1, error_msg/1]).

-type pattern_state() :: tuple().
-type module_name() :: chumak_pattern_req.

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

-callback peer_recv_message(State::pattern_state(), Message::chumak_protocol:message(), From::pid()) -> Reply::term().
-callback queue_ready(State::pattern_state(), Identity::string(), From::pid()) -> Reply::term().
-callback peer_disconected(State::pattern_state(), PeerPid::pid()) -> Reply::term().


%% @doc find matching pattern for a socket type.
-spec module(SocketType::socket_type()) -> module_name() | {error, invalid_socket_type}.
module(req)    -> chumak_req;
module(rep)    -> chumak_rep;
module(dealer) -> chumak_dealer;
module(router) -> chumak_router;
module(pub)    -> chumak_pub;
module(sub)    -> chumak_sub;
module(xpub)   -> chumak_xpub;
module(xsub)   -> chumak_xsub;
module(push)   -> chumak_push;
module(pull)   -> chumak_pull;
module(pair)   -> chumak_pair;
module(_)      -> {error, invalid_socket_type}.

%% @doc helper to translate reason atom to human-readable
error_msg(efsm) ->
    "operation cannot be performed on this socket at the moment".
