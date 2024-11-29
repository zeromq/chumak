%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Socket Type implementation for Erlang
%% @hidden

-module(chumak_socket).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-record(state, {socket,
                socket_state,
                socket_options = #{},
                identity}).

%% api behaviour
-export([start_link/2, stop/1]).

%% gen_server behaviors
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%% public API implementation
-spec start_link(Type::term(),
                 Identity::string()) ->
                        {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Type, Identity)
  when is_atom(Type),
       is_list(Identity) ->
    gen_server:start_link(?MODULE, {Type, Identity}, []).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).



%% gen_server implementation
init({Type, Identity}) ->
    process_flag(trap_exit, true), %% trap exit
    case code:ensure_loaded(Type) of
        {error, _} ->
            case chumak_pattern:module(Type) of
                {error, Reason} ->
                    {stop, Reason};
                ModuleName -> %% get the implementation of the type
                    {ok, S} = ModuleName:init(Identity),
                    {ok, #state{socket=ModuleName, socket_state=S, identity = Identity}}
            end;
        _ -> 
            {ok, S} = Type:init(Identity),
            {ok, #state{socket=Type, socket_state=S, identity = Identity}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({set_option, OptionName, OptionValue}, _From, State) ->
    set_option(OptionName, OptionValue, State);

handle_call({connect, Protocol, Host, Port, Resource}, _From, State) ->
    connect(Protocol, Host, Port, Resource, State);

handle_call({accept, SocketPid}, _From, State) ->
    accept(SocketPid, State);

handle_call({send, Data}, From, State) ->
    send(Data, From, State);

handle_call(recv, From, State) ->
    recv(From, State);

handle_call({send_multipart, Multipart}, From, State) ->
    send_multipart(Multipart, From, State);

handle_call(recv_multipart, From, State) ->
    recv_multipart(From, State);

handle_call(unblock, From, State) ->
    unblock(From, State);

handle_call({bind, tcp, Host, Port}, _From, State) ->
    Reply = chumak_bind:start_link(Host, Port), %% start a bind
    {reply, Reply, State};

handle_call(get_flags, _From, State) ->
    get_flags(State);

handle_call({bind, Protocol, _Host, _Port}, _From, State) ->
    {reply, {error, {unsupported_protocol, Protocol}}, State}.

handle_cast({peer_ready, From, Identity}, State) ->
    peer_ready(From, Identity, State);

handle_cast({subscribe, Topic}, State) ->
    pattern_support(State, subscribe, [Topic]);

handle_cast({cancel, Topic}, State) ->
    pattern_support(State, cancel, [Topic]);

handle_cast({peer_subscribe, From, Subscription}, State) ->
    pattern_support(State, peer_subscribe, [From, Subscription]);

handle_cast({peer_cancel_subscribe, From, Subscription}, State) ->
    pattern_support(State, peer_cancel_subscribe, [From, Subscription]);

handle_cast({peer_reconnected, From}, State) ->
    pattern_support(State, peer_reconnected, [From], nowarn);

handle_cast(CastMsg, State) ->
    ?LOG_ERROR("zmq system error", #{error => unhandled_handle_cast, args => CastMsg}),
    {noreply, State}.

handle_info({peer_recv_message, Message, From}, State) ->
    peer_recv_message(Message, From, State);

handle_info({queue_ready, Identity, From}, State) ->
    queue_ready(Identity, From, State);

%% When the client is crashed we should not exit
%% and we should let the implementation of type to deal with this
handle_info({'EXIT', PeerPid, _Other}, State) ->
    exit_peer(PeerPid, State);

handle_info(InfoMsg, State) ->
    ?LOG_ERROR("zmq system error", #{error => unhandled_handle_info, args => InfoMsg}),
    {noreply, State}.

terminate(Reason, #state{socket=Mod, socket_state=S}) ->
    Mod:terminate(Reason, S),
    ok.

%% private api

%% Take the sockets reply, and apply it to our state, while returning the reply.
store({reply, M, S}, State) -> {reply, M, State#state{socket_state=S}};
store({noreply, S}, State) -> {noreply, State#state{socket_state=S}}.

set_option(Name, Value, #state{socket_options = Options} = State)
  when Name =:= curve_server, is_boolean(Value);
       Name =:= curve_publickey, is_binary(Value);
       Name =:= curve_secretkey, is_binary(Value);
       Name =:= curve_serverkey, is_binary(Value) ->
    {reply, ok, State#state{socket_options = Options#{Name => Value}}};
set_option(Name, Value, #state{socket_options = Options} = State)
  when Name =:= curve_clientkeys ->
    case validate_keys(Value) of
        {ok, BinaryKeys} ->
            {reply, ok,
             State#state{socket_options = Options#{Name => BinaryKeys}}};
        {error, _Error} ->
            {reply, {error, einval}, State}
    end;
set_option(_Name, _Value, State) ->
    {reply, {error, einval}, State}.

connect(Protocol, Host, Port, Resource, #state{socket=S, socket_state=T}=State) ->
    {SocketType, PeerOpts} = peer_flags(State),

    case chumak_peer:connect(SocketType, Protocol, Host, Port, Resource, PeerOpts) of
        {ok, Pid} ->
            Reply = S:accept_peer(T, Pid),
            store(Reply, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

accept(SocketPid, #state{socket=S, socket_state=T}=State) ->
    {SocketType, PeerOpts} = peer_flags(State),

    case chumak_peer:accept(SocketType, SocketPid, PeerOpts) of
        {ok, Pid} ->
            Reply = S:accept_peer(T, Pid),
            store(Reply, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

send(Data, From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:send(T, Data, From),
    store(Reply, State).

recv(From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:recv(T, From),
    store(Reply, State).

send_multipart(Multipart, From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:send_multipart(T, Multipart, From),
    store(Reply, State).

recv_multipart(From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:recv_multipart(T, From),
    store(Reply, State).

unblock(From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:unblock(T, From),
    store(Reply, State).

get_flags(State) ->
    {reply, peer_flags(State), State}.

peer_ready(From, Identity, #state{socket=S, socket_state=T}=State) ->
    Reply = S:peer_ready(T, From, Identity),
    store(Reply, State).

pattern_support(State, Function, Args) ->
    pattern_support(State, Function, Args, warn).
%% check the implementation support function or not
pattern_support(#state{socket=S, socket_state=T}=State, Function, Args, Alert) ->
    IsExported = erlang:function_exported(S, Function, length(Args) + 1),

    case {IsExported, Alert} of
        {true, _} ->
            store(apply(S, Function, [T] ++ Args), State); %% call function and store new state

        {false, warn} ->
            ?LOG_ERROR("zmq system error", #{error => pattern_not_supported, socket => S, method => Function, args => Args}),
            {noreply, State};

        {false, _} ->
            {noreply, State}
    end.

peer_recv_message(Message, From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:peer_recv_message(T, Message, From),
    store(Reply, State).

queue_ready(Identity, From, #state{socket=S, socket_state=T}=State) ->
    Reply = S:queue_ready(T, Identity, From),
    store(Reply, State).

exit_peer(PeerPid, #state{socket=S,  socket_state=T}=State) ->
    Reply = S:peer_disconected(T, PeerPid),
    store(Reply, State).

peer_flags(#state{socket=Socket,
                  socket_state=SocketState,
                  socket_options=SocketOptions}) ->
    {SocketType, PeerOpts} = Socket:peer_flags(SocketState),
    Identity = Socket:identity(SocketState),
    {SocketType, lists:flatten([{identity, Identity},
                                maps:to_list(SocketOptions),
                                PeerOpts])}.

%% Make sure that kesy are either binaries or strings.
%% Strings should be Z85 encoded, convert those to binaries.
validate_keys(Keys) when is_list(Keys) ->
    validate_keys(Keys, []);
validate_keys(any) ->
    {ok, any};
validate_keys(_Other) ->
    {error, einval}.

validate_keys([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_keys([Key | T], Acc) when is_list(Key) ->
    try chumak_z85:decode(Key) of
        Binary ->
            validate_keys(T, [Binary | Acc])
    catch
        _:_ ->
            {error, "Failed to decode Z85 key"}
    end;
validate_keys([Key | T], Acc) when is_binary(Key) ->
    validate_keys(T, [Key | Acc]);
validate_keys(_, _) ->
    {error, "Invalid type for key"}.
