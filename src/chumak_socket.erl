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

%% @doc ZeroMQ Socket Type implementation for Erlang
%% @hidden

-module(chumak_socket).
-behaviour(gen_server).

-record(state, {socket, socket_state}).
%% api behaviour
-export([start_link/2]).

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



%% gen_server implementation
init({Type, Identity}) ->
    process_flag(trap_exit, true),
    case chumak_pattern:module(Type) of
        {error, Reason} ->
            {stop, Reason};
        ModuleName ->
            {ok, S} = ModuleName:init(Identity),
            {ok, #state{socket=ModuleName, socket_state=S}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

handle_call({bind, tcp, Host, Port}, _From, State) ->
    Reply = chumak_bind:start_link(Host, Port),
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
    error_logger:info_report([
                              unhandled_handle_cast,
                              {module, ?MODULE},
                              {msg, CastMsg}
                             ]),
    {noreply, State}.

handle_info({peer_recv_message, Message, From}, State) ->
    peer_recv_message(Message, From, State);

handle_info({queue_ready, Identity, From}, State) ->
    queue_ready(Identity, From, State);

handle_info({'EXIT', PeerPid, {shutdown, _Reason}}, State) ->
    exit_peer(PeerPid, State);

handle_info(InfoMsg, State) ->
    error_logger:info_report([
                              unhandled_handle_info,
                              {module, ?MODULE},
                              {msg, InfoMsg}
                             ]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% TODO: close all sockets
    ok.

%% private api

%% Take the sockets reply, and apply it to our state, while returning the reply.
store({reply, M, S}, State) -> {reply, M, State#state{socket_state=S}};
store({noreply, S}, State) -> {noreply, State#state{socket_state=S}}.

connect(Protocol, Host, Port, Resource, #state{socket=S, socket_state=T}=State) ->
    {SocketType, PeerOpts} = peer_flags(S, T),

    case chumak_peer:connect(SocketType, Protocol, Host, Port, Resource, PeerOpts) of
        {ok, Pid} ->
            Reply = S:accept_peer(T, Pid),
            store(Reply, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

accept(SocketPid, #state{socket=S, socket_state=T}=State) ->
    {SocketType, PeerOpts} = peer_flags(S, T),

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

get_flags(#state{socket=S, socket_state=T}=State) ->
    {reply, peer_flags(S, T), State}.

peer_ready(From, Identity, #state{socket=S, socket_state=T}=State) ->
    Reply = S:peer_ready(T, From, Identity),
    store(Reply, State).
 
pattern_support(State, Function, Args) ->
    pattern_support(State, Function, Args, warn).

pattern_support(#state{socket=S, socket_state=T}=State, Function, Args, Alert) ->
    IsExported = erlang:function_exported(S, Function, length(Args) + 1),

    case {IsExported, Alert} of
        {true, _} ->
            store(apply(S, Function, [T] ++ Args), State);

        {false, warn} ->
            error_logger:warning_report([
                                         pattern_not_supported,
                                         {module, S},
                                         {method, Function},
                                         {args, Args}
                                        ]),
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

peer_flags(Socket, SocketState) ->
    {SocketType, PeerOpts} = Socket:peer_flags(SocketState),
    Identity = Socket:identity(SocketState),
    {SocketType, [{identity, Identity} | PeerOpts]}.

