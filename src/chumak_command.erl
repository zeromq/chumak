%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Module responsible to decode and encode ZeroMQ commands
-module(chumak_command).
-include("chumak.hrl").

-export_type([command/0]).
-export([decode/2, command_name/1,
         encode_ready/4, ready_socket_type/1, ready_identity/1, ready_resource/1,
         ready_metadata/1, initiate_metadata/1, initiate_client_key/1,
         encode_error/1, error_reason/1,
         encode_subscribe/1, subscribe_subscription/1,
         encode_cancel/1, cancel_subscription/1,
         encode_ready_properties/1
        ]).

-record(ready, {
          socket_type=nil :: nil | string(),
          identity=""     :: string(),
          resource=""     :: string(),
          metadata=#{}    :: map()
         }).

-record(ping, {}).
-record(error, {reason :: string()}).
-record(subscribe, {subscription :: binary()}).
-record(cancel, {subscription :: binary()}).
-record(hello, {}).
-record(welcome, {}).
-record(initiate, {metadata = #{} :: map(),
                   client_public_key :: binary()}).
-record(message, {}).

-type ready()    :: ready().      %% ready command
-type ping()     :: ping().       %% ping command
-type error()    :: error().      %% error command
-type subscribe():: subscribe().  %% subscribe command
-type cancel()   :: cancel().     %% cancel command
%% commands used in the curveZMQ handshake:
-type hello()    :: #hello{}.
-type welcome()  :: #welcome{}.
-type initiate() :: #initiate{}.
%% CurveZMQ payload (messages) is also encoded as commands
-type message()  :: #message{}.


 %% commands available
-type command()  :: ready() | ping() | error() | subscribe() | cancel() |
                    hello() | welcome() | initiate() | ready() | message().

-type ready_decode_error() :: wrong_ready_message.
-type decode_error() :: ready_decode_error().

%%
%% Public API
%%

%% @doc decode reads incoming command and generate an command 'object'
-spec decode(Frame::chumak_protocol:frame(),
             CurveData::chumak_curve:curve_data()) ->
  {ok, Command::command(),
   NewCurveData::chumak_curve:curve_data()} | {error, decode_error()}.
decode(Frame, CurveData) ->
    <<CommandNameByteSize, Frame2/binary>> = Frame,
    CommandNameBitSize = 8 * CommandNameByteSize,
    <<CommandName:CommandNameBitSize/bitstring, CommandBody/binary>> = Frame2,
    decode_message(binary_to_atom(CommandName, utf8), CommandBody, CurveData).


%% @doc returns the name of a command
-spec command_name(Command::command()) -> Name::atom().
command_name(Command) ->
    element(1, Command).

%%
%% Ready command handler functions
%%

%% @doc return the socket type of a ready command
-spec ready_socket_type(Command::ready()) -> SocketType::atom().
ready_socket_type(#ready{socket_type=SocketType}) ->
    SocketType.


%% @doc return the identity of a ready command
-spec ready_identity(Command::ready()) -> Identity::string().
ready_identity(#ready{identity=Identity}) ->
    Identity.


%% @doc return the resource of a ready command
-spec ready_resource(Command::ready()) -> Resource::string().
ready_resource(#ready{resource=Resource}) ->
    Resource.


%% @doc return the metadata of a ready command
-spec ready_metadata(Command::ready()) -> Metadata::map().
ready_metadata(#ready{metadata=Metadata}) ->
    Metadata.

%% @doc return the metadata of an initiate command
-spec initiate_metadata(Command::initiate()) -> Metadata::map().
initiate_metadata(#initiate{metadata=Metadata}) ->
    Metadata.

%% @doc return the public key of the client. This is an element of the
%% INITIATE command. It can be used to identify the client.
-spec initiate_client_key(Command::initiate()) -> Key::binary().
initiate_client_key(#initiate{client_public_key=Value}) ->
    Value.


%% @doc encode a ready command
-spec encode_ready(SocketType::atom(), Identity::string(), Resource::string(), Metadata::map()) -> Data::binary().
encode_ready(SocketType, Identity, Resource, Metadata) when is_atom(SocketType) ->
    SocketTypeBin = string:to_upper(atom_to_list(SocketType)),
    Properties = lists:flatten([
                                {"Socket-Type", SocketTypeBin},
                                {"Identity", Identity},
                                {"Resource", Resource},
                                maps:to_list(Metadata)
                               ]),
    PropertiesFrame = encode_ready_properties(Properties),
    <<5, "READY", PropertiesFrame/binary>>.


%%
%% Error command functions
%%

%% @doc returns the reason of error
-spec error_reason(Command::error()) -> Reason::string().
error_reason(#error{reason=Reason}) ->
    Reason.

%% @doc returns an encoded error command
-spec encode_error(Reason::string()) -> Data::binary().
encode_error(Reason) when is_list(Reason) ->
    ReasonBin = list_to_binary(Reason),
    ReasonSize = byte_size(ReasonBin),
    <<5, "ERROR", ReasonSize, ReasonBin/binary>>.

%%
%% SUBSCRIBE functions
%%

%% @doc encode a subscribe command
-spec encode_subscribe(Subscription::binary()) -> Command::binary().
encode_subscribe(Subscription) when is_binary(Subscription) ->
    <<9, "SUBSCRIBE", Subscription/binary>>.

%% @doc return subscription of subscribe command
-spec subscribe_subscription(Command::subscribe()) -> Subscription::binary().
subscribe_subscription(#subscribe{subscription=Subscription}) ->
    Subscription.

%% @doc encode a cancel command
-spec encode_cancel(Subscription::binary()) -> Command::binary().
encode_cancel(Subscription) when is_binary(Subscription) ->
    <<6, "CANCEL", Subscription/binary>>.

%% @doc return subscription of cancel command
-spec cancel_subscription(Command::cancel()) -> Subscription::binary().
cancel_subscription(#cancel{subscription=Subscription}) ->
    Subscription.

%% Private API
%% Both the 'curve' and the 'null' mechanism use a READY message.
decode_message('READY', Body, #{mechanism := curve} = CurveData) ->
    try decode_curve_ready_message(Body, CurveData) of
        Result -> Result
    catch
        error:{badmatch,_Error} ->
            {error, wrong_ready_message}
    end;

decode_message('READY', Body, SecurityData) ->
    try decode_ready_message(#ready{}, Body) of
        Message -> {ok, Message, SecurityData}
    catch
        error:{badmatch,_} ->
            {error, wrong_ready_message}
    end;

decode_message('PING', _Body, SecurityData) ->
    {ok, #ping{}, SecurityData};

decode_message('ERROR', Body, SecurityData) ->
    try decode_error_message(Body) of
        Message -> {ok, Message, SecurityData}
    catch
        error:{badmatch,_} ->
            {error, wrong_error_message}
    end;

decode_message('SUBSCRIBE', Body, SecurityData) ->
    {ok, #subscribe{subscription=Body}, SecurityData};

decode_message('CANCEL', Body, SecurityData) ->
    {ok, #cancel{subscription=Body}, SecurityData};

decode_message('HELLO', Body, CurveData) ->
    try decode_hello_message(Body, CurveData) of
        Result -> Result
    catch
        %% TODO: ensure that client is disconnected in case the command
        %% malformed.
        error:{badmatch,_} ->
            {error, wrong_hello_message}
    end;

decode_message('WELCOME', Body, CurveData) ->
    try decode_welcome_message(Body, CurveData) of
        Result -> Result
    catch
        %% TODO: ensure that client is disconnected in case the command
        %% malformed.
        error:{badmatch,_} ->
            {error, wrong_welcome_message}
    end;

decode_message('INITIATE', Body, CurveData) ->
    try decode_initiate_message(Body, CurveData) of
        Result -> Result
    catch
        %% TODO: ensure that client is disconnected in case the command
        %% malformed.
        error:{badmatch,_} ->
            {error, wrong_initiate_message}
    end;

decode_message('MESSAGE', Body, CurveData) ->
    try decode_curve_message(Body, CurveData) of
        Result -> Result
    catch
        error:{badmatch,_} ->
            {error, wrong_message}
    end.

%% Decode Ready message utils
decode_ready_message(Command, Message) ->
    Properties = decode_ready_properties(Message),
    maps:fold(fun append_ready_property/3, Command, Properties).

append_ready_property("socket-type", SocketType, ReadyCommand) ->
    ReadyCommand#ready{
      socket_type=string:to_lower(SocketType)
     };
append_ready_property("identity", Identity, ReadyCommand) ->
    ReadyCommand#ready{identity=Identity};
append_ready_property("resource", Resource, ReadyCommand) ->
    ReadyCommand#ready{resource=Resource};
append_ready_property(Name, Value, #ready{metadata=MetaData}=ReadyCommand) ->
    ReadyCommand#ready{metadata=MetaData#{Name=>Value}}.

decode_ready_properties(MetaData) ->
    decode_ready_properties(MetaData, #{}).

decode_ready_properties(<<>>, Map) ->
    Map;
decode_ready_properties(<<PropertyNameLen, Rest/binary>>, Map) ->
    <<PropertyName:PropertyNameLen/binary,
      PropertyValueLen:32, Rest2/binary>> = Rest,
    <<PropertyValue:PropertyValueLen/binary, Rest3/binary>> = Rest2,
    Name = string:to_lower(binary_to_list(PropertyName)),
    Value = binary_to_list(PropertyValue),
    decode_ready_properties(Rest3, Map#{Name => Value}).

encode_ready_properties([]) ->
    <<"">>;
%%encode_ready_properties([{_Name, ""}|Properties]) ->
    %%encode_ready_properties(Properties);
encode_ready_properties([{Name, Value}|Properties]) ->
    NameBin = list_to_binary(Name),
    ValueBin = list_to_binary(Value),
    NameLen = byte_size(NameBin),
    ValueLen = byte_size(ValueBin),
    Tail = encode_ready_properties(Properties),
    <<NameLen, NameBin/binary, ValueLen:32, ValueBin/binary, Tail/binary>>.

decode_error_message(Body) ->
    <<Size, RemaingBody/binary>> = Body,
    <<Reason:Size/binary>> = RemaingBody,
    #error{reason=binary_to_list(Reason)}.

%% hello = %d5 "HELLO" version padding hello-client hello-nonce hello-box
%% hello-version = %x1 %x0     ; CurveZMQ major-minor version
%% hello-padding = 72%x00      ; Anti-amplification padding
%% hello-client = 32OCTET      ; Client public transient key C'
%% hello-nonce = 8OCTET        ; Short nonce, prefixed by "CurveZMQHELLO---"
%% hello-box = 80OCTET         ; Signature, Box [64 * %x0](C'->S)
decode_hello_message(<<1, 0, 0:72/integer-unit:8,
                       ClientPublicTransientKey:32/binary,
                       NonceBinary:8/binary,
                       HelloBox:80/binary>>,
                     #{curve_secretkey := ServerSecretPermanentKey} = CurveData) ->
    <<Nonce:8/integer-unit:8>> = NonceBinary,
    HelloNonceBinary = <<"CurveZMQHELLO---", NonceBinary/binary>>,
    %% TODO: Do something specific when decryption fails?
    {ok, <<0:64/integer-unit:8>>} =
        chumak_curve_if:box_open(HelloBox,
                                 HelloNonceBinary,
                                 ClientPublicTransientKey,
                                 ServerSecretPermanentKey),
    NewCurveData = CurveData#{client_nonce => Nonce,
                              client_public_transient_key => ClientPublicTransientKey},
    {ok, #hello{}, NewCurveData}.

%% ;   WELCOME command, 168 octets
%% welcome = %d7 "WELCOME" welcome-nonce welcome-box
%% welcome-nonce = 16OCTET     ; Long nonce, prefixed by "WELCOME-"
%% welcome-box = 144OCTET      ; Box [S' + cookie](S->C')
%% ;   This is the text sent encrypted in the box
%% cookie = cookie-nonce cookie-box
%% cookie-nonce = 16OCTET      ; Long nonce, prefixed by "COOKIE--"
%% cookie-box = 80OCTET        ; Box [C' + s'](K)
decode_welcome_message(<<NonceBinary:16/binary,
                         WelcomeBox:144/binary>>,
                     #{client_secret_transient_key := ClientSecretTransientKey,
                       curve_serverkey := ServerPublicPermanentKey
                       } = CurveData) ->
    WelcomeNonceBinary = <<"WELCOME-", NonceBinary/binary>>,
    %% TODO: Do something specific when decryption fails?
    {ok, <<ServerPublicTransientKey:32/binary, Cookie:96/binary>>} =
        chumak_curve_if:box_open(WelcomeBox,
                                 WelcomeNonceBinary,
                                 ServerPublicPermanentKey,
                                 ClientSecretTransientKey),
    NewCurveData = CurveData#{server_public_transient_key =>
                                  ServerPublicTransientKey,
                              cookie => Cookie},
    {ok, #welcome{}, NewCurveData}.

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
decode_initiate_message(<<CookieNonceBinary:16/binary,
                          CookieBox:80/binary,
                          NonceBinary:8/binary,
                          InitiateBox/binary>>,
                        #{
                          cookie_public_key := CookiePublicKey,
                          cookie_secret_key := CookieSecretKey,
                          client_nonce := OldClientNonce
                         } = CurveData) ->
    <<Nonce:8/integer-unit:8>> = NonceBinary,
    true = (Nonce > OldClientNonce),
    %% The cookie contains the server's secret transient key and
    %% the client public transient key.
    CookieNonce = <<"COOKIE--", CookieNonceBinary/binary>>,
    {ok, <<ClientPublicTransientKey:32/binary,
           ServerSecretTransientKey:32/binary>>} =
        chumak_curve_if:box_open(CookieBox,
                                 CookieNonce,
                                 CookiePublicKey,
                                 CookieSecretKey),
    InitiateNonceBinary = <<"CurveZMQINITIATE", NonceBinary/binary>>,
    {ok, <<ClientPublicPermanentKey:32/binary, VouchNonceBinary:16/binary,
           VouchBox:80/binary, MetaData/binary>>} =
        chumak_curve_if:box_open(InitiateBox, InitiateNonceBinary,
                                 ClientPublicTransientKey,
                                 ServerSecretTransientKey),
    %% The vouch must be decoded using the client permanent key,
    %% so that we know that the client also has the secret key, and
    %% therefore is who he claims to be.
    %% The vouch box (80 octets) encrypts the client's transient key C'
    %% (32 octets) and the server permanent key S (32 octets) from the
    %% client permanent key C to the server transient key S'.
    VouchNonce = <<"VOUCH---", VouchNonceBinary/binary>>,
    {ok, <<ClientPublicTransientKey:32/binary,
           _ServerPublicPermanentKey:32/binary>>} =
        chumak_curve_if:box_open(VouchBox, VouchNonce,
                                 ClientPublicPermanentKey,
                                 ServerSecretTransientKey),
    NewCurveData = CurveData#{client_nonce => Nonce,
                              server_secret_transient_key =>
                                  ServerSecretTransientKey,
                              client_public_permanent_key =>
                                  ClientPublicPermanentKey,
                              client_public_transient_key =>
                                  ClientPublicTransientKey,
                              cookie_secret_key => <<>>,
                              cookie_public_key => <<>>},
    {ok, #initiate{metadata = decode_ready_properties(MetaData),
                   client_public_key = ClientPublicPermanentKey},
     NewCurveData}.

%% ;   READY command, 30+ octets
%% ready = %d5 "READY" ready-nonce ready-box
%% ready-nonce = 8OCTET        ; Short nonce, prefixed by "CurveZMQREADY---"
%% ready-box = 16*OCTET        ; Box [metadata](S'->C')
decode_curve_ready_message(
        <<NonceBinary:8/binary,
          ReadyBox/binary>>,
        #{server_public_transient_key := ServerPublicTransientKey,
          client_secret_transient_key := ClientSecretTransientKey
         } = CurveData) ->
    <<Nonce:8/integer-unit:8>> = NonceBinary,
    ReadyNonceBinary = <<"CurveZMQREADY---", NonceBinary/binary>>,
    {ok, MetaData} =
        chumak_curve_if:box_open(ReadyBox, ReadyNonceBinary,
                                 ServerPublicTransientKey, ClientSecretTransientKey),
    NewCurveData = CurveData#{server_nonce => Nonce},
    {ok, #ready{metadata = decode_ready_properties(MetaData)}, NewCurveData}.

%% ;   MESSAGE command, 33+ octets
%% message = %d7 "MESSAGE" message_nonce message-box
%% message-nonce = 8OCTET      ; Short nonce, prefixed by "CurveZMQMESSAGE-"
%% message-box = 17*OCTET      ; Box [payload](S'->C') or (C'->S')
%% ;   This is the text sent encrypted in the box
%% payload = payload-flags payload-data
%% payload-flags = OCTET       ; Explained below
%% payload-data = *octet       ; 0 or more octets
decode_curve_message(
        <<NonceBinary:8/binary,
          MessageBox/binary>>,
        #{role := server,
          client_public_transient_key := PublicKey,
          server_secret_transient_key := SecretKey,
          client_nonce := OldNonceValue
         } = CurveData) ->
    <<Nonce:8/integer-unit:8>> = NonceBinary,
    true = (Nonce > OldNonceValue),
    MessageNonce = <<"CurveZMQMESSAGEC", NonceBinary/binary>>,
    {ok, <<_, MessageData/binary>>} =
        chumak_curve_if:box_open(MessageBox, MessageNonce,
                                 PublicKey, SecretKey),
    NewCurveData = CurveData#{client_nonce => Nonce},
    {ok, MessageData, NewCurveData};
decode_curve_message(
        <<NonceBinary:8/binary,
          MessageBox/binary>>,
        #{role := client,
          server_public_transient_key := PublicKey,
          client_secret_transient_key := SecretKey,
          server_nonce := OldNonceValue
         } = CurveData) ->
    <<Nonce:8/integer-unit:8>> = NonceBinary,
    true = (Nonce > OldNonceValue),
    MessageNonce = <<"CurveZMQMESSAGES", NonceBinary/binary>>,
    {ok, <<_, MessageData/binary>>} =
        chumak_curve_if:box_open(MessageBox, MessageNonce,
                                 PublicKey, SecretKey),
    NewCurveData = CurveData#{server_nonce => Nonce},
    {ok, MessageData, NewCurveData}.
