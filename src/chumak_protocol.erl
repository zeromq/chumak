%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Parser of ZeroMQ protocol
%%
%% This module was created to make responsibility of decode and make a buffer of ZeroMQ wire protocol,
%% the client of this module needs to announce that more bytes were been received by peer.
%%

-module(chumak_protocol).
-include("chumak.hrl").

-export_type([decoder/0]).
-export([build_greeting_frame/2,
         build_hello_frame/1,
         build_welcome_frame/1,
         build_initiate_frame/2,
         build_ready_frame/2,
         new_decoder/1, 
         decoder_state/1, decoder_version/1, decoder_buffer/1, 
         decoder_mechanism/1, decoder_as_server/1,
         decoder_security_data/1, set_decoder_security_data/2,
         decode/2, continue_decode/1,
         encode_old_subscribe/1, encode_old_cancel/1,
         encode_command/1, encode_message_multipart/3, 
         encode_more_message/3, encode_last_message/3,
         message_data/1, message_has_more/1
        ]).

-define(PROTOCOL_MAJOR, 3).
-define(PROTOCOL_MINOR, 1).
-define(SIGNATURE_SIZE, 11).

%% ENUM for frame type
-define(SMALL_LAST_MESSAGE, 0).
-define(SMALL_MORE_MESSAGE, 1).
-define(LARGE_LAST_MESSAGE, 2).
-define(LARGE_MORE_MESSAGE, 3).
-define(SMALL_COMMAND, 4).
-define(LARGE_COMMAND, 6).

-record(decoder, {
          state=initial     :: decoder_state(), %% the state of decoder
          size=0            :: integer(),
          buffer=nil        :: nil | binary(),
          next_state=nil    :: nil | decoder_state(),
          version_major=nil :: nil | {some, integer()},       %% number of the major version
          version_minor=nil :: nil | {some, integer()},       %% number of the minor version
          security_data=nil :: decoder_security_data(),
          mechanism=nil     :: nil | security_mechanism(),
          as_server=false   :: boolean()
         }).

-record(message,{
          frame    :: binary(),
          has_more :: true | false
         }).

-type message() :: #message{}.
-type decoder() :: #decoder{}. %% the structure responsible to decode incoming bytes
-type decoder_state() :: 'initial' | 'waiting_minor_version' | 'waiting_mechanism' |
                         'waiting_as_server' | 'waiting_filler' | 'ready' |
                         'command_ready' | 'message_ready' | 'require_size'. %% the state of decoder
-type decoder_version() :: {MajorVersion::integer(), MinorVersion::integer()}.
-type decoder_security_data() :: #{} | chumak_curve:curve_data().
-type frame() :: binary().  %% the bytes received or sent
-type invalid_version() :: {invalid_version, Major::atom()}.
-type invalid_mechanism() :: {mechanism_not_supported_yet, Mechanism::atom()}.
-type bad_greeting_frame() :: {bad_greeting_frame, Frame::binary()}.
-type decode_reason() :: bad_greeting_frame() | invalid_version() | invalid_mechanism(). %% decode fail reason

-type decoder_ready() :: {ready, UpdatedDecoder::decoder()}. %% returned when decoder was finished greeting part.
-type decoder_ok()    :: {ok, UpdatedDecoder::decoder()}.    %% returned when decoder only decoded the frame.
-type decoder_cmds()  :: {ok, UpdatedDecoder::decoder(), [Command::term()]}. %% returned when decoder was found one or more commands.
-type decoder_error() :: {error, Reason::decode_reason()}.
-type decoder_reply() :: decoder_ready() | decoder_ok() | decoder_cmds() | decoder_error(). %% reply of decoder command


%%
%% Public API
%%

%% @doc build_greeting_frame creates a new greeting frame done to send to peer.
-spec build_greeting_frame(AsServer::boolean(),
                           Mechanism::chumak:security_mechanism()) -> Frame::frame().
build_greeting_frame(AsServer, Mechanism) ->
    Padding = <<0:64>>, %% Padding has size 8
    Signature = <<16#ff, Padding/binary, 16#7f>>, %% Signature has size 10
    MechanismBinary = encode_mechanism(Mechanism),
    AsServerBinary = case AsServer of
                          false ->
                              <<8#0>>;
                          true ->
                              <<8#1>>
                      end,
    Filler = binary:copy(<<8#0>>, 31),
    <<Signature/binary, ?PROTOCOL_MAJOR, ?PROTOCOL_MINOR, MechanismBinary/binary, 
      AsServerBinary/binary, Filler/binary>>.


%% The HELLO Command
%% 
%% The first command on a CurveZMQ connection is the HELLO command. The client
%% SHALL send a HELLO command after opening the stream connection. This command
%% SHALL be 200 octets long and have the following fields:
%%
%% The command ID, which is [5]"HELLO".
%%
%% The CurveZMQ version number, which SHALL be the two octets 1 and 0.
%% 
%% An anti-amplification padding field. This SHALL be 70 octets, all zero. This
%% filler field ensures the HELLO command is larger than the WELCOME command,
%% so an attacker who spoofs a sender IP address cannot use the server to
%% overwhelm that innocent 3rd party with response data.
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
%%
%% NOTE: the size of 70 for the padding is wrong, must be 72.

%% HELLO command, 200 octets
%% hello = %d5 "HELLO" version padding hello-client hello-nonce hello-box
%% hello-version = %x1 %x0     ; CurveZMQ major-minor version
%% hello-padding = 72%x00      ; Anti-amplification padding
%% hello-client = 32OCTET      ; Client public transient key C'
%% hello-nonce = 8OCTET        ; Short nonce, prefixed by "CurveZMQHELLO---"
%% hello-box = 80OCTET         ; Signature, Box [64 * %x0](C'->S)
%% NOTE: there is 1 additional byte for the size of the command name,
%% so it adds up to 200.
build_hello_frame(#{client_public_transient_key := ClientPublicTransientKey, 
                    client_secret_transient_key := ClientSecretTransientKey,
                    curve_serverkey := ServerPermanentKey,
                    client_nonce := ClientShortNonce} = SecurityData) ->
    CommandName = <<"HELLO">>, 
    CommandNameSize = <<5>>,
    Version = <<1, 0>>, 
    Padding = <<0:72/integer-unit:8>>,
    ClientShortNonceBinary = <<ClientShortNonce:8/integer-unit:8>>,
    Nonce = <<"CurveZMQHELLO---", ClientShortNonceBinary/binary>>,
    %% box(Msg, Nonce, Pk, Sk)
    SignatureBox = 
        chumak_curve_if:box(<<0:64/integer-unit:8>>, Nonce, ServerPermanentKey,
                            ClientSecretTransientKey),
    Command = <<CommandNameSize/binary, CommandName/binary, Version/binary, 
                Padding/binary, ClientPublicTransientKey/binary, 
                ClientShortNonceBinary/binary, SignatureBox/binary>>,
    {encode_command(Command), 
     SecurityData#{client_nonce => ClientShortNonce + 1}}.

%% The WELCOME Command
%%
%% The server SHALL respond to a valid HELLO command with a WELCOME command.
%% This command SHALL be 168 octets long and have the following fields:
%%
%% The command ID, which is [7]"WELCOME".
%%
%% A server long nonce (16 octets). This nonce SHALL be implicitly prefixed by
%% the 8 characters "WELCOME-" to form a 24-octet nonce used to encrypt and
%% decrypt the welcome box.
%%
%% A welcome box (144 octets) that encrypts the server public transient key S'
%% (32 octets) and the server cookie (96 octets), from the server permanent key
%% S to the client's transient key C'.
%%
%% Note that the server uses a 16-octet "long nonce" in the WELCOME command and
%% when creating a cookie. This nonce SHALL be unique for this server permanent
%% key. The recommended simplest strategy is to use 16 random octets from a
%% sufficiently good entropy source.
%%
%% The cookie consists of two fields:
%%
%% A server long nonce (16 octets). This nonce SHALL be implicitly prefixed by
%% the 8 characters @@"COOKIE--"@@ to form a 24-octet nonce used to encrypt and
%% decrypt the cookie box.
%%
%% A cookie box (80 octets) that holds the client public transient key C' (32
%% octets) and the server secret transient key s' (32 octets), encrypted to and
%% from a secret short-term "cookie key". The server MUST discard from its
%% memory the cookie key after a short interval, for example 60 seconds, or as
%% soon as the client sends a valid INITIATE command.
%%
%% The server SHALL generate a new cookie for each WELCOME command it sends.
%%
%% The client SHALL validate all fields and SHALL disconnect from servers that
%% send malformed WELCOME commands.
%%
%% ;   WELCOME command, 168 octets
%% welcome = %d7 "WELCOME" welcome-nonce welcome-box
%% welcome-nonce = 16OCTET     ; Long nonce, prefixed by "WELCOME-"
%% welcome-box = 144OCTET      ; Box [S' + cookie](S->C')
%% ;   This is the text sent encrypted in the box
%% ?? the Server public transient key ?? (bug in the spec).
%% cookie = cookie-nonce cookie-box
%% cookie-nonce = 16OCTET      ; Long nonce, prefixed by "COOKIE--"
%% cookie-box = 80OCTET        ; Box [C' + s'](K)
build_welcome_frame(#{server_public_transient_key := ServerPublicTransientKey, 
                      server_secret_transient_key := ServerSecretTransientKey,
                      curve_secretkey := ServerSecretPermanentKey,
                      client_public_transient_key := ClientPublicTransientKey,
                      cookie_public_key := CookiePublicKey,
                      cookie_secret_key := CookieSecretKey} = CurveData) ->
    LongNonce = chumak_curve_if:randombytes(16),
    WelcomeBoxNonce = <<"WELCOME-", LongNonce/binary>>,
    CookieBoxNonce = <<"COOKIE--", LongNonce/binary>>,
    CookieBox = 
        chumak_curve_if:box(<<ClientPublicTransientKey/binary,
                              ServerSecretTransientKey/binary>>,
                            CookieBoxNonce, CookiePublicKey, CookieSecretKey),
    Cookie = <<LongNonce/binary, CookieBox/binary>>,
    WelcomeBox = 
        chumak_curve_if:box(<<ServerPublicTransientKey/binary, Cookie/binary>>, 
                            WelcomeBoxNonce, 
                            ClientPublicTransientKey, ServerSecretPermanentKey),
    Welcome = <<7, <<"WELCOME">>/binary, LongNonce/binary, WelcomeBox/binary>>,
    %% remove the transient keys (they will be returned by the client in the
    %% initiate message).
    NewCD = CurveData#{server_public_transient_key => <<>>,
                       server_secret_transient_key => <<>>,
                       client_public_transient_key => <<>>},
    {encode_command(Welcome), NewCD}.

%% The INITIATE Command
%%
%% When the client receives a WELCOME command it can decrypt this to receive
%% the server's transient key S', and the cookie, which it must send back to
%% the server. The cookie is the only memory of the server's secret transient
%% key s'.
%%
%% The client SHALL respond to a valid WELCOME with an INITIATE command. This
%% command SHALL be at least 257 octets long and have the following fields:
%%
%% - The command ID, which is [8]"INITIATE".
%%
%% - The cookie provided by the server in the WELCOME command (96 octets).
%%
%% - A client short nonce (8 octets). The nonce SHALL be implicitly prefixed with
%% the 16 characters "CurveZMQINITIATE" to form the 24-octet nonce used to
%% encrypt and decrypt the vouch box.
%%
%% - The initiate box (144 or more octets), by which the client securely sends
%% its permanent public key C to the server. The initiate box holds the client
%% permanent public key C (32 octets), the vouch (96 octets), and the metadata
%% (0 or more octets), encrypted from the client's transient key C' to the
%% server's transient key S'.
%%
%% The vouch itself consists of two fields:
%%
%% - A client long nonce (16 octets). This nonce SHALL be implicitly prefixed
%% with the 8 characters @@"VOUCH---"@@ to give a 24-octet nonce, used to
%% encrypt and decrypt the vouch box. This nonce SHALL be unique for all
%% INITIATE commands from this client permanent key. A valid strategy is to use
%% 16 random octets from a sufficiently good entropy source.
%%
%% - The vouch box (80 octets), that encrypts the client's transient key C' (32
%% octets) and the server permanent key S (32 octets) from the client permanent
%% key C to the server transient key S'.
%%
%% The metadata consists of a list of properties consisting of name and value
%% as size-specified strings. The name SHALL be 1 to 255 characters. Zero-sized
%% names are not valid. The case (upper or lower) of names SHALL NOT be
%% significant. The value SHALL be 0 to 2^31-1 octets of opaque binary data.
%% Zero-sized values are allowed. The semantics of the value depend on the
%% property. The value size field SHALL be four octets, in network order. Note
%% that this size field will mostly not be aligned in memory.
%%
%% The server SHALL validate all fields and SHALL reject and disconnect clients
%% who send malformed INITIATE commands.
%%
%% After decrypting the INITIATE command, the server MAY authenticate the
%% client based on its permanent public key C. If the client does not pass
%% authentication, the server SHALL not respond except by closing the
%% connection. If the client passes authentication the server SHALL send a
%% READY command and MAY then immediately send MESSAGE commands.

%% ;   INITIATE command, 257+ octets
%% initiate = %d8 "INITIATE" cookie initiate-nonce initiate-box
%% initiate-cookie = cookie    ; Server-provided cookie
%% initiate-nonce = 8OCTET     ; Short nonce, prefixed by "CurveZMQINITIATE"
%% initiate-box = 144*OCTET    ; Box [C + vouch + metadata](C'->S')
%% ;   This is the text sent encrypted in the box
%% vouch = vouch-nonce vouch-box
%% vouch-nonce = 16OCTET       ; Long nonce, prefixed by "VOUCH---"
%% vouch-box = 80OCTET         ; Box [C',S](C->S')
%% metadata = *property
%% property = name value
%% name = OCTET *name-char
%% name-char = ALPHA | DIGIT | "-" | "_" | "." | "+"
%% value = value-size value-data
%% value-size = 4OCTET         ; Size in network order
%% value-data = *OCTET         ; 0 or more octets
build_initiate_frame(MetaData,
                     #{client_nonce := ClientShortNonce, 
                       client_public_transient_key := ClientPublicTransientKey,
                       client_secret_transient_key := ClientSecretTransientKey,
                       curve_publickey := ClientPublicPermanentKey,
                       curve_secretkey := ClientSecretPermanentKey,
                       server_public_transient_key := ServerPublicTransientKey,
                       curve_serverkey := ServerPublicPermanentKey,
                       cookie := Cookie} = CurveData) ->
    ClientShortNonceBinary = <<ClientShortNonce:8/integer-unit:8>>,
    LongNonce = chumak_curve_if:randombytes(16),
    VouchBoxNonce = <<"VOUCH---", LongNonce/binary>>,
    InitiateBoxNonce = <<"CurveZMQINITIATE", ClientShortNonceBinary/binary>>,

    %% The vouch box (80 octets), that encrypts the client's transient key C'
    %% (32 octets) and the server permanent key S (32 octets) from the client
    %% permanent key C to the server transient key S'.
    VouchBox = 
        chumak_curve_if:box(<<ClientPublicTransientKey/binary,
                              ServerPublicPermanentKey/binary>>,
                            VouchBoxNonce, 
                            ServerPublicTransientKey,
                            ClientSecretPermanentKey),

    Vouch = <<LongNonce/binary, VouchBox/binary>>,
    MetaDataBinary = chumak_command:encode_ready_properties(MetaData),

    %% The initiate box holds the client permanent public key C (32 octets),
    %% the vouch (96 octets), and the metadata (0 or more octets), encrypted
    %% from the client's transient key C' to the server's transient key S'.
    InitiateBox = 
        chumak_curve_if:box(<<ClientPublicPermanentKey/binary, Vouch/binary,
                              MetaDataBinary/binary>>,
                            InitiateBoxNonce, 
                            ServerPublicTransientKey,
                            ClientSecretTransientKey),
    Initiate = <<8, <<"INITIATE">>/binary, Cookie/binary, 
                 ClientShortNonceBinary/binary, InitiateBox/binary>>,
    {encode_command(Initiate), CurveData#{client_nonce => ClientShortNonce + 1}}.


%% The READY Command
%%
%% The server SHALL respond to a valid INITIATE command with a READY command.
%% This command SHALL be at least 30 octets long and have the following fields:
%%
%% The command ID, which is [5]"READY".
%%
%% A server short nonce (8 octets). The nonce SHALL be implicitly prefixed with
%% the 16 characters @@"CurveZMQREADY---"@@ to form the 24-octet nonce used to
%% encrypt and decrypt the ready box.
%%
%% The ready box (16 or more octets). This shall contain metadata of the same
%% format as sent in the INITIATE command, encrypted from the server's
%% transient key S' to the client's transient key C'.
%%
%% The client SHALL validate all fields and SHALL disconnect from servers who
%% send malformed READY commands.
%%
%% The client MAY validate the meta-data. If the client accepts the meta-data,
%% it SHALL then expect MESSAGE commands from the server.
%%
%% Note that the server uses an 8 octet "short nonce" in the HELLO, INITIATE,
%% and MESSAGE commands. This nonce SHALL be an incrementing integer, and
%% unique to each command within a connection. The server SHALL NOT send more
%% than 2^64-1 commands in one connection. The client SHALL verify that a
%% server connection does use correctly incrementing short nonces, and SHALL
%% disconnect from servers that reuse a short nonce.
%%
%% NOTE: I am assuming that the list of commands above should be READY and 
%% MESSAGE.
%%
%% ;   READY command, 30+ octets
%% ready = %d5 "READY" ready-nonce ready-box
%% ready-nonce = 8OCTET        ; Short nonce, prefixed by "CurveZMQREADY---"
%% ready-box = 16*OCTET        ; Box [metadata](S'->C')
build_ready_frame(MetaData,
                  #{client_public_transient_key := ClientPublicTransientKey,
                    server_secret_transient_key := ServerSecretTransientKey,
                    server_nonce := ServerShortNonce} = CurveData) ->
    ServerShortNonceBinary = <<ServerShortNonce:8/integer-unit:8>>,
    ReadyBoxNonce = <<"CurveZMQREADY---", ServerShortNonceBinary/binary>>,

    MetaDataBinary = chumak_command:encode_ready_properties(MetaData),

    %% The ready box (16 or more octets). This shall contain metadata of the
    %% same format as sent in the INITIATE command, encrypted from the server's
    %% transient key S' to the client's transient key C'.
    ReadyBox = 
        chumak_curve_if:box(<<MetaDataBinary/binary>>,
                            ReadyBoxNonce, ClientPublicTransientKey, 
                            ServerSecretTransientKey),

    Ready = <<5, "READY", ServerShortNonceBinary/binary,
                 ReadyBox/binary>>,
    {encode_command(Ready), CurveData#{server_nonce => ServerShortNonce + 1}}.

%% ;   MESSAGE command, 33+ octets
%% message = %d7 "MESSAGE" message_nonce message-box
%% message-nonce = 8OCTET      ; Short nonce, prefixed by "CurveZMQMESSAGE-"
%% message-box = 17*OCTET      ; Box [payload](S'->C') or (C'->S')
%% ;   This is the text sent encrypted in the box
%% payload = payload-flags payload-data
%% payload-flags = OCTET       ; Explained below
%% payload-data = *octet       ; 0 or more octets
build_message(Message, 
              curve,
              #{role := client,
                client_secret_transient_key := SecretKey,
                server_public_transient_key := PublicKey,
                client_nonce := ClientNonce} = SecurityData) ->
    NonceBinary = <<ClientNonce:8/integer-unit:8>>,
    Nonce = <<"CurveZMQMESSAGEC", NonceBinary/binary>>,
    MessageBox = 
        chumak_curve_if:box(<<0, Message/binary>>, 
                            Nonce, PublicKey, SecretKey),
    {<<7, "MESSAGE", NonceBinary/binary, MessageBox/binary>>,
     SecurityData#{client_nonce => ClientNonce + 1}};
build_message(Message, 
              curve,
              #{role := server,
                server_secret_transient_key := SecretKey,
                client_public_transient_key := PublicKey,
                server_nonce := ServerNonce} = SecurityData) ->
    NonceBinary = <<ServerNonce:8/integer-unit:8>>,
    Nonce = <<"CurveZMQMESSAGES", NonceBinary/binary>>,
    MessageBox = 
        chumak_curve_if:box(<<0, Message/binary>>, 
                            Nonce, PublicKey, SecretKey),
    {<<7, "MESSAGE", NonceBinary/binary, MessageBox/binary>>,
     SecurityData#{server_nonce => ServerNonce + 1}};
build_message(Message, _, SecurityData) ->
    {Message, SecurityData}.

%% @doc new_decoder creates a new decoder waiting for greeting message
-spec new_decoder(SecurityData::decoder_security_data()) -> NewDecoder::decoder().
new_decoder(SecurityData) ->
    #decoder{security_data = SecurityData}.


%% @doc decode reads incoming frame and generate an updated decoder
-spec decode(Decoder::decoder(), Frame::frame()) -> decoder_reply().
%% waiting the command, its first for performance reasons
decode(#decoder{state=ready}=Decoder, Frame) ->
    <<Flag:1/binary, _/binary>> = Frame,

    NextState = case Flag of
                    %% if bit 2 is enabled then is a traffic for a command
                    <<_:5, 1:1, _:1, _:1>> ->
                        command_ready;

                    %% if bit 2 is disabled then is a traffic for a message
                    <<_:5, 0:1, _:1, _:1>> ->
                        message_ready
                end,
    case Frame of
        %% if bit 1 is disabled then is a small frame
        <<_:5, _:1, 0:1, _:1, Size, _RemaingFrame/binary>> ->
            require_size(Decoder, Size+2, Frame, NextState);

        %% if bit 1 is enabled then is a large frame
        <<_:5, _:1, 1:1, _:1, Size:64, _RemaingFrame/binary>> ->
            require_size(Decoder, Size+9, Frame, NextState);

        %% if size is remaining for small frame
        <<_:5, _:1, 0:1, _:1>> ->
            require_size(Decoder, 2, Frame, ready);

        %% if size is remaining for large frame
        <<_:5, _:1, 1:1, _:1, _TooShortToBeSize/binary>> ->
            require_size(Decoder, 9, Frame, ready);

        X ->
            {error, {bad_ready_packet, X}}
    end;

%% waiting the command to be buffered
decode(#decoder{state=command_ready}=Decoder, Frame) ->
    case Frame of
        <<?SMALL_COMMAND, CommandSize, RemaingFrame/binary>> ->
            decode_command(Decoder, CommandSize, RemaingFrame);

        <<?LARGE_COMMAND, CommandSize:64, RemaingFrame/binary>> ->
            decode_command(Decoder, CommandSize, RemaingFrame);

        X ->
            {error, {bad_command_packet, X}}
    end;


%% waiting the message to be buffered
decode(#decoder{state=message_ready}=Decoder, Frame) ->
    case Frame of
        %% if bit 1 is disabled then is a small message
        <<_:5, _:1, 0:1, More:1, Size, RemaingFrame/binary>> ->
            decode_message(Decoder, Size, RemaingFrame, More);

        %% if bit 1 is enabled then is a large message
        <<_:5, _:1, 1:1, More:1, Size:64, RemaingFrame/binary>> ->
            decode_message(Decoder, Size, RemaingFrame, More);

        X ->
            {error, {bad_message_packet, X}}
    end;


%% waits the size of buffer
decode(#decoder{state=require_size}=Decoder, Frame) ->
    CurrentBuffer = Decoder#decoder.buffer,
    require_size(Decoder, Decoder#decoder.size, <<CurrentBuffer/binary, Frame/binary>>, Decoder#decoder.next_state);


%% waiting the signature step
decode(#decoder{state=initial}=Decoder, Frame) when byte_size(Frame) >= ?SIGNATURE_SIZE->
    case Frame of
        <<16#ff, _Padding:64/bitstring, 16#7f, VersionMajor, RemaingFrame/binary>> when VersionMajor >= ?PROTOCOL_MAJOR ->
            continue_decode(Decoder#decoder{state=waiting_minor_version, version_major={some, VersionMajor}}, RemaingFrame);
        <<16#ff, _Padding:64/bitstring, 16#7f, VersionMajor, _RemaingFrame/binary>> ->
            {error, {invalid_version, VersionMajor}};
        X ->
            {error, {bad_greeting_frame, X}}
    end;
decode(#decoder{state=initial}=Decoder, Frame) ->
    require_size(Decoder, ?SIGNATURE_SIZE, Frame, initial);


%% waiting the version step
decode(#decoder{state=waiting_minor_version}=Decoder, Frame) ->
    case Frame of
        <<VersionMinor, RemaingFrame/binary>> ->
            continue_decode(Decoder#decoder{state=waiting_mechanism, version_minor={some, VersionMinor}}, RemaingFrame);
        X ->
            {error, {bad_greeting_frame, X}}
    end;

%% waiting mechanism step
decode(#decoder{state=waiting_mechanism}=Decoder, Frame) ->
    case Frame of
        <<Mechanism:160/bitstring, RemaingFrame/binary>> ->
            case strip_binary_to_atom(Mechanism) of
                'NULL' ->
                    continue_decode(Decoder#decoder{state=waiting_as_server,
                                                    mechanism = null}, RemaingFrame);
                'PLAIN' ->
                    {error, {mechanism_not_supported_yet, 'PLAIN'}};
                'CURVE' ->
                    continue_decode(Decoder#decoder{state=waiting_as_server,
                                                    mechanism = curve}, RemaingFrame);
                MechanismType ->
                    {error, {invalid_mechanism, MechanismType}}
            end;
        X ->
            {error, {bad_greeting_frame, X}}
    end;

%% waiting for as server
decode(#decoder{state=waiting_as_server}=Decoder, Frame) ->
    case Frame of
        <<AsServer, RemaingFrame/binary>> ->
            AsServerBool = case AsServer of 
                               1 -> true;
                               0 -> false
                           end,
            continue_decode(Decoder#decoder{state=waiting_filler,
                                            as_server = AsServerBool}, 
                            RemaingFrame)
    end;

%% waiting for filler
decode(#decoder{state=waiting_filler}=Decoder, Frame) ->
    case Frame of
        <<0:248, RemaingFrame/binary>> ->
            {ready, Decoder#decoder{state=ready, buffer=RemaingFrame}}
    end.

%% @doc continue decoding after the decoder announce decoder has finished the greetings part
-spec continue_decode(Decoder::decoder()) -> decoder_reply().
continue_decode(#decoder{buffer=Buffer}=Decoder) when is_binary(Buffer) ->
    continue_decode(Decoder#decoder{buffer=nil}, Buffer).

%% @doc decoder_state returns the current status of decoder
-spec decoder_state(Decoder::decoder()) -> State::decoder_state().
decoder_state(#decoder{state=State}) ->
    State.

%% @doc decoder_version returns the current version of decoder
-spec decoder_version(Decoder::decoder()) -> State::decoder_version().
decoder_version(#decoder{version_major={some, VersionMajor}, version_minor={some, VersionMinor}}) ->
    {VersionMajor, VersionMinor}.

%% @doc decoder_mechanism returns the current security mechanism
-spec decoder_mechanism(Decoder::decoder()) -> security_mechanism().
decoder_mechanism(#decoder{mechanism=Mechanism}) ->
    Mechanism.

%% @doc decoder_as_server returns the as_server setting from the greeting
-spec decoder_as_server(Decoder::decoder()) -> boolean().
decoder_as_server(#decoder{as_server=AsServer}) ->
    AsServer.

%% @doc Return the current buffer of a decoder
-spec decoder_buffer(Decoder::decoder()) -> binary() | nil.
decoder_buffer(#decoder{buffer=Buffer}) ->
    Buffer.


%% @doc Return the security data of a decoder
-spec decoder_security_data(Decoder::decoder()) -> nil | chumak_curve:curve_data().
decoder_security_data(#decoder{security_data=SecurityData}) ->
    SecurityData.

%% @doc Set the security data of a decoder
-spec set_decoder_security_data(
          Decoder::decoder(), 
          SecurityData::decoder_security_data()) -> decoder().
set_decoder_security_data(Decoder, SecurityData) ->
    Decoder#decoder{security_data = SecurityData}.

%% @doc Generate a traffic based com a command
-spec encode_command(Command::binary()) -> Traffic::binary().
encode_command(Command) when is_binary(Command) ->
    encode_frame(?SMALL_COMMAND, ?LARGE_COMMAND, Command).

%% @doc encode a old format of subscriptions found in version 3.0 of zeromq
-spec encode_old_subscribe(Topic::binary()) -> Traffic::binary().
encode_old_subscribe(Topic) ->
    {Encoded, _} = encode_last_message(<<1, Topic/binary>>, null, #{}),
    Encoded.

%% @doc encode a old format of unsubscriptions found in version 3.0 of zeromq
-spec encode_old_cancel(Topic::binary()) -> Traffic::binary().
encode_old_cancel(Topic) ->
    {Encoded, _} = encode_last_message(<<0, Topic/binary>>, null, #{}),
    Encoded.

%% @doc Generate a traffic based com a message with multi parts
-spec encode_message_multipart([Message::binary()],
                               Mechanism::security_mechanism(),
                               SecurityData::map()) -> {Traffic::binary(),
                                                        NewSecurityData::map()}.
%% TODO: check this - is this how multipart messages work?
encode_message_multipart(Multipart, Mechanism, SecurityData) ->
    More = lists:sublist(Multipart, length(Multipart) -1),
    Last = lists:last(Multipart),
    {MoreTrafficList, NewSecurityData} = 
        lists:mapfoldl(fun(Part, SDataAcc) ->
                           encode_more_message(Part, Mechanism, SDataAcc)
                       end, SecurityData, More),
    MoreTraffic = binary:list_to_bin(MoreTrafficList),
    {LastTraffic, FinalSecData} = encode_last_message(Last, Mechanism, 
                                                      NewSecurityData),
    {<<MoreTraffic/binary, LastTraffic/binary>>, FinalSecData}.

%% @doc Generate a traffic based on a message
-spec encode_more_message(Message::binary(),
                          Mechanism::security_mechanism(),
                          SecurityData::map()) -> {Traffic::binary(), map()}.
encode_more_message(Message, Mechanism, SecurityData) 
  when is_binary(Message) ->
    {Frame, NewSecurityData} = build_message(Message, 
                                             Mechanism,
                                             SecurityData),
    {encode_frame(?SMALL_MORE_MESSAGE, ?LARGE_MORE_MESSAGE, Frame),
     NewSecurityData}.

%% @doc Generate a traffic based on a message
-spec encode_last_message(Message::binary(),
                          Mechanism::security_mechanism(),
                          SecurityData::map()) -> {Traffic::binary(), map()}.
encode_last_message(Message, Mechanism, SecurityData) 
  when is_binary(Message) ->
    {Frame, NewSecurityData} = build_message(Message, 
                                             Mechanism,
                                             SecurityData),
    {encode_frame(?SMALL_LAST_MESSAGE, ?LARGE_LAST_MESSAGE, Frame),
     NewSecurityData}.

%% @doc Return data for a message
-spec message_data(Message::message()) -> Frame::binary().
message_data(#message{frame=Data}) ->
    Data.

%% @doc Return if message has the flag marked to receive more messages
-spec message_has_more(Message::message()) -> true | false.
message_has_more(#message{has_more=HasMore}) ->
    HasMore.

%%
%% Private API
%%

continue_decode(Decoder, <<>>) ->
    {ok, Decoder};
continue_decode(Decoder, Frame) ->
    decode(Decoder, Frame).


decode_command(#decoder{security_data = SecurityData} = Decoder, 
               Size, Frame) ->
    <<CommandFrame:Size/binary, RemaingFrame/binary>> = Frame,

    case chumak_command:decode(CommandFrame, SecurityData) of
        {ok, Command, NewSecurityData} ->
            accumule_commands(Decoder#decoder{security_data = NewSecurityData},
                              Command, RemaingFrame);
        {error, _} = Error ->
            Error
    end.

decode_message(#decoder{security_data = SecurityData} = Decoder, 
               Size, Frame, MoreFlag) ->
    HasMore = case MoreFlag of
                  1 ->
                      true;
                  0 ->
                      false
              end,
    <<MessageFrame:Size/binary, RemaingFrame/binary>> = Frame,
    {DecodedFrame, NewSecurityData} = decode_frame(MessageFrame, SecurityData),
    accumule_commands(Decoder#decoder{security_data = NewSecurityData}, 
                      #message{frame=DecodedFrame, has_more=HasMore},
                      RemaingFrame).

decode_frame(CurveMessage, #{mechanism := curve} = SecurityData) ->
    case chumak_command:decode(CurveMessage, SecurityData) of
        {ok, Message, NewSecurityData} ->
            {Message, NewSecurityData}
    end;
decode_frame(Frame, SecurityData) ->
    {Frame, SecurityData}.


accumule_commands(Decoder, Command, Frame) ->
    ReadyDecoder = Decoder#decoder{state=ready},
    case continue_decode(ReadyDecoder, Frame) of
        {ok, UpdatedDecoder} ->
            {ok, UpdatedDecoder, [Command]};
        {ok, UpdatedDecoder, Commands} ->
            {ok, UpdatedDecoder, [Command|Commands]};
        Error ->
            Error
    end.


%% when length of Frame are validated
require_size(Decoder, Size, Frame, NextState) when byte_size(Frame) >= Size->
    decode(Decoder#decoder{state=NextState, buffer=nil}, Frame);
require_size(Decoder, Size, Frame, NextState) ->
    {ok, Decoder#decoder{state=require_size, size=Size, buffer=Frame, next_state=NextState}}.


strip_binary_to_atom(Bin) ->
    [T, _] = binary:split(Bin, <<0>>),
    binary_to_atom(T, utf8).

%% encode small frame
encode_frame(SmallFlag, _LargeFlag, Frame) when byte_size(Frame) < 256 ->
    Size = byte_size(Frame),
    <<SmallFlag, Size, Frame/binary>>;

%% encode large frame
encode_frame(_SmallFlag, LargeFlag, Frame) ->
    Size = byte_size(Frame),
    SizeFrame = binary:encode_unsigned(Size),
    Padding = binary:copy(<<0>>, 8-byte_size(SizeFrame)),
    <<LargeFlag, Padding/binary, SizeFrame/binary, Frame/binary>>.

%% The security mechanism is an ASCII string, null-padded as needed to fit 
%% 20 octets. 
encode_mechanism(null) ->
    << <<"NULL">>/binary, 0:128>>;
encode_mechanism(curve) ->
    << <<"CURVE">>/binary, 0:120>>.
