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

%% @doc Module responsible to decode and encode ZeroMQ commands
-module(chumak_command).
-include("chumak.hrl").

-export_type([command/0]).
-export([decode/1, command_name/1,
         encode_ready/4, ready_socket_type/1, ready_identity/1, ready_resource/1, ready_metadata/1,
         encode_error/1, error_reason/1,
         encode_subscribe/1, subscribe_subscription/1,
         encode_cancel/1, cancel_subscription/1
        ]).

-record(ready, {
          socket_type=nil :: nil | socket_type(),
          identity=""     :: string(),
          resource=""     :: string(),
          metadata=#{}    :: map()
         }).

-record(ping, {}).
-record(error, {reason :: string()}).
-record(subscribe, {subscription :: binary()}).
-record(cancel, {subscription :: binary()}).

-type ready()    :: ready().      %% ready command
-type ping()     :: ping().       %% ping command
-type error()    :: error().      %% error command
-type subscribe():: subscribe().  %% subscribe command
-type cancel()   :: cancel().     %% cancel command

 %% commands available
-type command()  :: ready() | ping() | error() | subscribe() | cancel ().

-type ready_decode_error() :: wrong_ready_message.
-type decode_error() :: ready_decode_error().

%%
%% Public API
%%

%% @doc decode reads incoming command and generate an command 'object'
-spec decode(Frame::chumak_protocol:frame()) -> Command::command() | {error, decode_error()}.
decode(Frame) ->
    <<CommandNameByteSize, Frame2/binary>> = Frame,
    CommandNameBitSize = 8 * CommandNameByteSize,
    <<CommandName:CommandNameBitSize/bitstring, CommandBody/binary>> = Frame2,
    decode_message(binary_to_atom(CommandName, utf8), CommandBody).


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

%% @doc returns an encoded errorr command
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
decode_message('READY', Body) ->
    try decode_ready_message(#ready{}, Body) of
        Message -> {ok, Message}
    catch
        error:{badmatch,_} ->
            {error, wrong_ready_message}
    end;

decode_message('PING', _Body) ->
    {ok, #ping{}};

decode_message('ERROR', Body) ->
    try decode_error_message(Body) of
        Message -> {ok, Message}
    catch
        error:{badmatch,_} ->
            {error, wrong_error_message}
    end;

decode_message('SUBSCRIBE', Body) ->
    {ok, #subscribe{subscription=Body}};

decode_message('CANCEL', Body) ->
    {ok, #cancel{subscription=Body}}.

%% Decode Ready message utils
decode_ready_message(Command, <<"">>) ->
    Command;

decode_ready_message(Command, <<PropertyNameLen, Part1/binary>>) ->
    PropertyNameBitLen = PropertyNameLen * 8,
    <<PropertyName:PropertyNameBitLen/bitstring, Part2/binary>> = Part1,

    <<PropertyValueLen:32, Part3/binary>> = Part2,
    PropertyValueBitLen = PropertyValueLen * 8,
    <<PropertyValue:PropertyValueBitLen/bitstring, Part4/binary>> = Part3,

    Name = string:to_lower(binary_to_list(PropertyName)),
    Value = binary_to_list(PropertyValue),
    UpdatedCommand = append_ready_property(Command, Name, Value),
    decode_ready_message(UpdatedCommand, Part4).

append_ready_property(ReadyCommand, "socket-type", SocketType) ->
    ReadyCommand#ready{
      socket_type=list_to_atom(string:to_lower(SocketType))
     };
append_ready_property(ReadyCommand, "identity", Identity) ->
    ReadyCommand#ready{identity=Identity};
append_ready_property(ReadyCommand, "resource", Resource) ->
    ReadyCommand#ready{resource=Resource};
append_ready_property(#ready{metadata=MetaData}=ReadyCommand, Name, Value) ->
    ReadyCommand#ready{metadata=MetaData#{Name=>Value}}.

encode_ready_properties([]) ->
    <<"">>;
encode_ready_properties([{_Name, ""}|Properties]) ->
    encode_ready_properties(Properties);
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
