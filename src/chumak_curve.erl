%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Functions to support curve security
%%
%% Where possible this implementation follows the API that is described
%% in https://github.com/zeromq/zeromq4-1/blob/master/doc/zmq_curve.txt

-module(chumak_curve).
-include("chumak.hrl").

%% To become a CURVE server, the application sets the ZMQ_CURVE_SERVER
%% option on the socket, and then sets the ZMQ_CURVE_SECRETKEY option
%% to provide the socket with its long-term secret key. The application does
%% not provide the socket with its long-term public key, which is used only by
%% clients.
%%
%% To become a CURVE client, the application sets the ZMQ_CURVE_SERVERKEY
%% option with the long-term public key of the server it intends to connect to,
%% or accept connections from, next. The application then sets the
%% ZMQ_CURVE_PUBLICKEY and ZMQ_CURVE_SECRETKEY options with its client
%% long-term key pair.

-type curve_data() :: #{
        mechanism => curve,
        role => client | server,
        curve_publickey => binary(),  %% server does not need its own public key
        curve_secretkey => binary(),  %% available for both roles, client or server
        curve_serverkey => binary(),  %% client needs sever public key.
        client_nonce => integer(),    %% both sides have a nonce
        server_nonce => integer(),
        client_public_transient_key => binary(),
        client_secret_transient_key => binary(),
        server_public_transient_key => binary(),
        server_secret_transient_key => binary(),
        cookie_public_key => binary(),
        cookie_secret_key => binary()}.

-export_type([curve_data/0]).

-define(SMALL_COMMAND, 4).
-define(LARGE_COMMAND, 6).

%% API
-export([security_handshake/4]).

%% @doc Execute the curveZMQ security handshake, in accordance with
%% https://rfc.zeromq.org/spec:26/CURVEZMQ/
%%
%% See also https://gist.github.com/sysbot/4b23e9765f1fd13ec5aa
-spec security_handshake(Socket::gen_tcp:socket(),
                         Decoder::chumak_protocol:decoder(),
                         AsServer::boolean(),
                         Metadata::term()) ->
    {chumak_protocol:decoder(), {ok, chumak_peer:handshake_data()} |
                                {error, term()}}.

security_handshake(Socket, Decoder, false, Metadata) ->
    %% Client role. Send HELLO to server.
    try
        CurveData = chumak_protocol:decoder_security_data(Decoder),
        {ok, CurveData2} = validate_client_curve_data(CurveData),
        {ok, CurveData3} = send_hello_step(Socket, CurveData2),
        Decoder2 = chumak_protocol:set_decoder_security_data(Decoder,
                                                             CurveData3),
        %% Receive the Welcome Frame.
        %% Length is 2 bytes command-size + 168 bytes payload = 170 bytes.
        {ok, WelcomeFrame} = gen_tcp:recv(Socket, 170, ?GREETINGS_TIMEOUT),
        {ok, Decoder3, [_Welcome]} = chumak_protocol:decode(Decoder2,
                                                            WelcomeFrame),
        CurveData4 = chumak_protocol:decoder_security_data(Decoder3),
        {ok, CurveData5} = send_initiate_step(Socket, CurveData4, Metadata),
        Decoder4 = chumak_protocol:set_decoder_security_data(Decoder,
                                                             CurveData5),
        {ok, ReadyFrame} = receive_command(Socket),
        {ok, Decoder5, [Ready]} = chumak_protocol:decode(Decoder4, ReadyFrame),
        MetaData = chumak_command:ready_metadata(Ready),
        CurveData6 = chumak_protocol:decoder_security_data(Decoder5),
        {Decoder5, {ready, MetaData#{security_data => CurveData6}}}
    catch
        error:{badmatch, {error, Reason}} ->
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => Reason}),
            {Decoder, {error, Reason}};
        error:{badmatch, Error} ->
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => Error}),
            {Decoder, {error, Error}}
    end;
security_handshake(Socket, Decoder, true, Metadata) ->
    %% Server role. Wait for HELLO from client.
    try
        CurveData = chumak_protocol:decoder_security_data(Decoder),
        {ok, #{curve_clientkeys := AllowedClients} = CurveData2} =
            validate_server_curve_data(CurveData),
        Decoder2 = chumak_protocol:set_decoder_security_data(Decoder,
                                                             CurveData2),

        %% The HELLO Frame is a "normal" command, so the command length is
        %% is part of the frame. This means 2 bytes in addition to the 200 that
        %% are in the spec.
        {ok, HelloFrame} = gen_tcp:recv(Socket, 202, ?GREETINGS_TIMEOUT),
        {ok, Decoder3, [_Hello]} = chumak_protocol:decode(Decoder2, HelloFrame),
        CurveData3 = chumak_protocol:decoder_security_data(Decoder3),

        %% Send WELCOME
        {ok, CurveData4} = send_welcome_step(Socket, CurveData3),
        Decoder4 = chumak_protocol:set_decoder_security_data(Decoder3,
                                                             CurveData4),
        %% Receive INITIATE
        {ok, InitiateFrame} = receive_command(Socket),
        {ok, Decoder5, [Initiate]} = chumak_protocol:decode(Decoder4,
                                                            InitiateFrame),

        %% If required, authenticate the client.
        case AllowedClients of
            any ->
                ok;
            _ ->
                ClientKey = chumak_command:initiate_client_key(Initiate),
                true = lists:member(ClientKey, AllowedClients)
        end,

        MetaData = chumak_command:initiate_metadata(Initiate),

        CurveData5 = chumak_protocol:decoder_security_data(Decoder5),

        %% Send READY
        {ok, CurveData6} = send_ready_step(Socket, CurveData5, Metadata),
        Decoder6 = chumak_protocol:set_decoder_security_data(Decoder5,
                                                             CurveData6),
        {Decoder6, {ready, MetaData#{security_data => CurveData6}}}
    catch
        error:{badmatch, {error, Reason}} ->
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => Reason}),
            {Decoder, {error, Reason}};
        error:{badmatch, Error} ->
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => Error}),
            {Decoder, {error, Error}}
    end.

receive_command(Socket) ->
    case gen_tcp:recv(Socket, 1, ?GREETINGS_TIMEOUT) of
        {ok, <<?SMALL_COMMAND>>} ->
            {ok, <<Size>>} = gen_tcp:recv(Socket, 1, ?GREETINGS_TIMEOUT),
            {ok, Frame} = gen_tcp:recv(Socket, Size, ?GREETINGS_TIMEOUT),
            {ok, <<?SMALL_COMMAND, Size, Frame/binary>>};
        {ok, <<?LARGE_COMMAND>>} ->
            {ok, <<Size:64>>} = gen_tcp:recv(Socket, 8, ?GREETINGS_TIMEOUT),
            {ok, Frame} = gen_tcp:recv(Socket, Size, ?GREETINGS_TIMEOUT),
            {ok, <<?LARGE_COMMAND, Size:64, Frame/binary>>};
        {error, _} = Error ->
            Error
    end.

%% The HELLO Command
%%
%% The first command on a CurveZMQ connection is the HELLO command. The client
%% SHALL send a HELLO command after opening the stream connection. This command
%%
%%
%% The client's public transient key C' (32 octets). The client SHALL generate
%% a unique key pair for each connection it creates to a server. It SHALL
%% discard this key pair when it closes the connection, and it MUST NOT store
%% its secret key in permanent storage, nor share it in any way.
%%
%% A client short nonce (8 octets). The nonce SHALL be implicitly prefixed with
%% the 16 characters @@"CurveZMQHELLO---"@@ to form the 24-octet nonce used to
%% encrypt and decrypt the signature box.
%%
%% The signature box (80 octets). This SHALL contain 64 zero octets, encrypted
%% from the client's transient key C' to the server's permanent key S.
%%
%% The server SHALL validate all fields and SHALL reject and disconnect clients
%% who send malformed HELLO commands.
%%
%% When the server gets a valid HELLO command, it SHALL generate a new
%% transient key pair, and encode both the public and secret key in a WELCOME
%% command, as explained below. The server SHALL not keep this transient key
%% pair and SHOULD keep minimal state for the client until the client responds
%% with a valid INITIATE command. This protects against denial-of-service
%% attacks where unauthenticated clients send many HELLO commands to consume
%% server resources.
%%
%% Note that the client uses an 8 octet "short nonce" in the HELLO, INITIATE,
%% and MESSAGE commands. This nonce SHALL be an incrementing integer, and
%% unique to each command within a connection. The client SHALL NOT send more
%% than 2^64-1 commands in one connection. The server SHALL verify that a
%% client connection does use correctly incrementing short nonces, and SHALL
%% disconnect clients that reuse a short nonce.

send_hello_step(Socket, CurveData) ->
    {Hello, NewCurveData} = chumak_protocol:build_hello_frame(CurveData),
    case gen_tcp:send(Socket, Hello) of
        ok ->
            {ok, NewCurveData};
        {error, SendReason} ->
            {error, {send_hello_error, SendReason}}
    end.

%% The WELCOME Command
%%
%% The server SHALL respond to a valid HELLO command with a WELCOME command.
%% who send malformed HELLO commands.
%%
%% When the server gets a valid HELLO command, it SHALL generate a new
%% transient key pair, and encode both the public and secret key in a WELCOME
%% command, as explained below. The server SHALL not keep this transient key
%% pair and SHOULD keep minimal state for the client until the client responds
%% with a valid INITIATE command. This protects against denial-of-service
%% attacks where unauthenticated clients send many HELLO commands to consume
%% server resources.
%%
%% Note that the client uses an 8 octet "short nonce" in the HELLO, INITIATE,
%% and MESSAGE commands. This nonce SHALL be an incrementing integer, and
%% unique to each command within a connection. The client SHALL NOT send more
%% than 2^64-1 commands in one connection. The server SHALL verify that a
%% client connection does use correctly incrementing short nonces, and SHALL
%% disconnect clients that reuse a short nonce.

send_welcome_step(Socket, CurveData) ->
    {Welcome, Cookie} = chumak_protocol:build_welcome_frame(CurveData),
    case gen_tcp:send(Socket, Welcome) of
        ok ->
            {ok, Cookie};
        {error, SendReason} ->
            {error, {send_hello_error, SendReason}}
    end.


send_initiate_step(Socket, CurveData, Metadata) ->
    {Initiate, NewCurveData} =
        chumak_protocol:build_initiate_frame(Metadata, CurveData),
    case gen_tcp:send(Socket, Initiate) of
        ok ->
            {ok, NewCurveData};
        {error, SendReason} ->
            {error, {send_initiate_error, SendReason}}
    end.

send_ready_step(Socket, CurveData, Metadata) ->
    {Ready, NewCurveData} = chumak_protocol:build_ready_frame(Metadata,
                                                              CurveData),
    case gen_tcp:send(Socket, Ready) of
        ok ->
            {ok, NewCurveData};
        {error, SendReason} ->
            {error, {send_ready_error, SendReason}}
    end.

%% curve_serverkey must be set
validate_client_curve_data(CurveData) when is_map(CurveData) ->
    true = is_binary(maps:get(curve_serverkey, CurveData, undefined)),
    #{public := PK, secret := SK} = chumak_curve_if:box_keypair(),
    {ok, CurveData#{mechanism => curve,
                    role => client,
                    client_secret_transient_key => SK,
                    client_public_transient_key => PK,
                    client_nonce => 1}}.

%% curve_secretkey must be set,
validate_server_curve_data(CurveData) when is_map(CurveData) ->
    true = is_binary(maps:get(curve_secretkey, CurveData, undefined)),
    ClientKeys = maps:get(curve_clientkeys, CurveData, any),
    #{public := PK, secret := SK} = chumak_curve_if:box_keypair(),
    #{public := Cookie_PK, secret := Cookie_SK} = chumak_curve_if:box_keypair(),
    {ok, CurveData#{mechanism => curve,
                    curve_clientkeys => ClientKeys,
                    role => server,
                    server_secret_transient_key => SK,
                    server_public_transient_key => PK,
                    cookie_public_key => Cookie_PK,
                    cookie_secret_key => Cookie_SK,
                    server_nonce => 1}}.
