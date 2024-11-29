%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Pattern behaviour for Erlang
%%
%% This behaviour defines all methods that a pattern needs to implement.

-module(chumak_pattern).
-include("chumak.hrl").

-export([module/1, error_msg/1]).

-type pattern_state() :: map().

-callback valid_peer_type(SocketType::socket_type()) -> valid | invalid.
-callback init(Identity::string()) -> {ok, pattern_state()}.
-callback terminate(Reason::term(), State::pattern_state()) -> ok.
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

-callback unblock(State::pattern_state(), From::term()) -> Reply::term().


%% @doc find matching pattern for a socket type.
-spec module(SocketType::socket_type()) -> module() | {error, invalid_socket_type}.
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
