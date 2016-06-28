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

%% @doc ZeroMQ Socket Type implementation for Erlang
%% @hidden

-module(erlangzmq_socket).
-behaviour(gen_server).

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
    case erlangzmq_pattern:module(Type) of
        {error, Reason} ->
            {stop, Reason};
        ModuleName ->
            ModuleName:init(Identity)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({connect, Protocol, Host, Port, Resource}, _From, State) ->
    Module = module(State),
    {SocketType, PeerOpts} = peer_flags(State),

    case erlangzmq_peer:connect(SocketType, Protocol, Host, Port, Resource, PeerOpts) of
        {ok, Pid} ->
            Module:accept_peer(State, Pid);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({accept, SocketPid}, _From, State) ->
    Module = module(State),
    {SocketType, PeerOpts} = peer_flags(State),

    case erlangzmq_peer:accept(SocketType, SocketPid, PeerOpts) of
        {ok, Pid} ->
            Module:accept_peer(State, Pid);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({send, Data}, From, State) ->
    Module = module(State),
    Module:send(State, Data, From);

handle_call(recv, From, State) ->
    Module = module(State),
    Module:recv(State, From);


handle_call({send_multipart, Multipart}, From, State) ->
    Module = module(State),
    Module:send_multipart(State, Multipart, From);

handle_call(recv_multipart, From, State) ->
    Module = module(State),
    Module:recv_multipart(State, From);

handle_call({bind, tcp, Host, Port}, _From, State) ->
    Reply = erlangzmq_bind:start_link(Host, Port),
    {reply, Reply, State};

handle_call(get_flags, _From, State) ->
    {reply, peer_flags(State), State};

handle_call({bind, Protocol, _Host, _Port}, _From, State) ->
    {reply, {error, {unsupported_protocol, Protocol}}, State}.

handle_cast({peer_ready, From, Identity}, State) ->
    Module = module(State),
    Module:peer_ready(State, From, Identity);

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
    Module = module(State),
    Module:peer_recv_message(State, Message, From);

handle_info({queue_ready, Identity, From}, State) ->
    Module = module(State),
    Module:queue_ready(State, Identity, From);

handle_info({'EXIT', PeerPid, {shutdown, _Reason}}, State) ->
    Module = module(State),
    Module:peer_disconected(State, PeerPid);

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
module(State) ->
    %% module pattern that handling this request MUST to be first value
    %% of the state
    element(1, State).

identity(State) ->
    %% identity of pattern that handling this request MUST to be second value
    %% of the state
    element(2, State).

peer_flags(State) ->
    Module = module(State),
    {SocketType, PeerOpts} = Module:peer_flags(State),
    Identity = identity(State),
    {SocketType, [{identity, Identity} | PeerOpts]}.

pattern_support(State, Function, Args) ->
    pattern_support(State, Function, Args, warn).

pattern_support(State, Function, Args, Alert) ->
    Module = module(State),
    IsExported = erlang:function_exported(Module, Function, length(Args) + 1),

    case {IsExported, Alert} of
        {true, _} ->
            apply(Module, Function, [State] ++ Args);

        {false, warn} ->
            error_logger:warning_report([
                                         pattern_not_supported,
                                         {module, Module},
                                         {method, Function},
                                         {args, Args}
                                        ]),
            {noreply, State};

        {false, _} ->
            {noreply, State}
    end.
