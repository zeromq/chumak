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

%% @doc Parser of ZeroMQ protocol
%%
%% This module was created to make responsability of decode and make a buffer of ZeroMQ wire protocol,
%% the client of this module needs to anounce that more bytes were been received by peer.
%%

-module(erlangzmq_protocol).

-export_type([decoder/0]).
-export([build_greeting_frame/0,
         new_decoder/0, decoder_state/1, decoder_version/1, decoder_buffer/1, decode/2, continue_decode/1,
         encode_old_subscribe/1, encode_old_cancel/1,
         encode_command/1, encode_message_multipart/1, encode_more_message/1, encode_last_message/1,
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
          state=initial :: decoder_state(), %% the state of decoder
          size=0        :: integer(),
          buffer=nil    :: binary(),
          next_state    :: decoder_state(),
          version_major :: integer(),       %% number of the major version
          version_minor :: integer()        %% number of the minor version
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
-type decoder_version() :: {MajorVersion::integer(), MinorVersion::integer}.
-type frame() :: binary().  %% the bytes received or sent
-type invalid_version() :: {invalid_version, Major::atom()}.
-type invalid_mechanism() :: {mechanism_not_supported_yet, Mechanism::atom()}.
-type bad_greeting_frame() :: {bad_greeting_frame, Frame::binary}.
-type decode_reason() :: bad_greeting_frame() | invalid_version() | invalid_mechanism(). %% decode fail reason

-type decoder_ready() :: {ready, UpdatedDecoder::decoder()}. %% returned when decoder was finished greeting part.
-type decoder_ok()    :: {ok, UpdatedDecoder::decoder()}.    %% returned when decoder only decoded the frame.
-type decoder_cmds()  :: {ok, UpdatedDecoder::decoder(), [Command::list()]}. %% returned when decoder was found one or more commands.
-type decoder_error() :: {error, Reason::decode_reason()}.
-type decoder_reply() :: decoder_ready() | decoder_ok() | decoder_cmds() | decoder_error(). %% reply of decoder command


%%
%% Public API
%%

%% @doc build_greeting_frame creates a new greeting frame done to send to peer.
-spec build_greeting_frame() -> Frame::frame().
build_greeting_frame() ->
    Padding = <<0:64>>, %% Padding has size 8
    Signature = <<16#ff, Padding/binary, 16#7f>>, %% Signature has size 10
    Null = <<"NULL">>,
    Mechanism = <<Null/binary, 0:128>>, %% Mechanism has size 20
    AsServer = <<8#0>>,
    Filler = binary:copy(<<8#0>>, 31),
    <<Signature/binary, ?PROTOCOL_MAJOR, ?PROTOCOL_MINOR, Mechanism/binary, AsServer/binary, Filler/binary>>.


%% @doc new_decoder creates a new decoder waiting for greeting message
-spec new_decoder() -> NewDecoder::decoder().
new_decoder() ->
    #decoder{}.


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

        %% if size is remaing for small frame
        <<_:5, _:1, 0:1, _:1>> ->
            require_size(Decoder, 2, Frame, ready);

        %% if size is remaing for large frame
        <<_:5, _:1, 1:1, _:1>> ->
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
            continue_decode(Decoder#decoder{state=waiting_minor_version, version_major=VersionMajor}, RemaingFrame);
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
            continue_decode(Decoder#decoder{state=waiting_mechanism, version_minor=VersionMinor}, RemaingFrame);
        X ->
            {error, {bad_greeting_frame, X}}
    end;

%% waiting mechanism step
decode(#decoder{state=waiting_mechanism}=Decoder, Frame) ->
    case Frame of
        <<Mechanism:160/bitstring, RemaingFrame/binary>> ->
            case strip_binary_to_atom(Mechanism) of
                'NULL' ->
                    continue_decode(Decoder#decoder{state=waiting_as_server}, RemaingFrame);
                'PLAIN' ->
                    {error, {mechanism_not_supported_yet, 'PLAIN'}};
                'CURVE' ->
                    {error, {mechanism_not_supported_yet, 'CURVE'}};
                MechanismType ->
                    {error, {invalid_mechanism, MechanismType}}
            end;
        X ->
            {error, {bad_greeting_frame, X}}
    end;

%% waiting for as server
decode(#decoder{state=waiting_as_server}=Decoder, Frame) ->
    case Frame of
        <<_AsServer, RemaingFrame/binary>> ->
            continue_decode(Decoder#decoder{state=waiting_filler}, RemaingFrame)
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
decoder_version(#decoder{version_major=VersionMajor, version_minor=VersionMinor}) ->
    {VersionMajor, VersionMinor}.

%% @doc Return the current buffer of a decoder
-spec decoder_buffer(Decoder::decoder()) -> binary() | nil.
decoder_buffer(#decoder{buffer=Buffer}) ->
    Buffer.

%% @doc Generate a traffic based com a command
-spec encode_command(Command::binary()) -> Traffic::binary().
encode_command(Command) when is_binary(Command) ->
    encode_frame(?SMALL_COMMAND, ?LARGE_COMMAND, Command).

%% @doc encode a old format of subscriptions found in version 3.0 of zeromq
-spec encode_old_subscribe(Topic::binary()) -> Traffic::binary().
encode_old_subscribe(Topic) ->
    encode_last_message(<<1, Topic/binary>>).

%% @doc encode a old format of unsubscriptions found in version 3.0 of zeromq
-spec encode_old_cancel(Topic::binary()) -> Traffic::binary().
encode_old_cancel(Topic) ->
    encode_last_message(<<0, Topic/binary>>).

%% @doc Generate a traffic based com a message with multi parts
-spec encode_message_multipart([Message::binary()]) -> Traffic::binary().
encode_message_multipart(Multipart) ->
    More = lists:sublist(Multipart, length(Multipart) -1),
    Last = lists:last(Multipart),

    MoreTraffic = binary:list_to_bin(lists:map(fun encode_more_message/1, More)),
    LastTraffic = encode_last_message(Last),

    <<MoreTraffic/binary, LastTraffic/binary>>.

%% @doc Generate a traffic based on a message
-spec encode_more_message(Message::binary()) -> Traffic::binary().
encode_more_message(Message) when is_binary(Message) ->
    encode_frame(?SMALL_MORE_MESSAGE, ?LARGE_MORE_MESSAGE, Message).

%% @doc Generate a traffic based on a message
-spec encode_last_message(Message::binary()) -> Traffic::binary().
encode_last_message(Message) when is_binary(Message) ->
    encode_frame(?SMALL_LAST_MESSAGE, ?LARGE_LAST_MESSAGE, Message).

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


decode_command(Decoder, Size, Frame) ->
    <<CommandFrame:Size/binary, RemaingFrame/binary>> = Frame,

    case erlangzmq_command:decode(CommandFrame) of
        {ok, Command} ->
            accumule_commands(Decoder, Command, RemaingFrame);
        Error ->
            Error
    end.

decode_message(Decoder, Size, Frame, MoreFlag) ->
    HasMore = case MoreFlag of
                  1 ->
                      true;
                  0 ->
                      false
              end,

    <<MessageFrame:Size/binary, RemaingFrame/binary>> = Frame,
    Message = #message{frame=MessageFrame, has_more=HasMore},
    accumule_commands(Decoder, Message, RemaingFrame).


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
