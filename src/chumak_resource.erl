%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Resource Router implementation for Erlang
%% @hidden

-module(chumak_resource).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% api behaviour
-export([start_link/0]).

%% gen_server behaviors
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%% public API implementation
-spec start_link() -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link() ->
    gen_server:start_link(?MODULE, {}, []).


-record(state, {
          resources :: map(),
          monitors :: map()
}).

%% gen_server implementation
init(_Args) ->
    process_flag(trap_exit, true),
    State = #state{
               resources=#{},
               monitors=#{}
              },
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({accept, SocketPid}, _From, State) ->
    case chumak_peer:accept(none, SocketPid, [multi_socket_type]) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({bind, tcp, Host, Port}, _From, State) ->
    Reply = chumak_bind:start_link(Host, Port),
    {reply, Reply, State};

handle_call({bind, Protocol, _Host, _Port}, _From, State) ->
    {reply, {error, {unsupported_protocol, Protocol}}, State};

handle_call({route_resource, Resource}, _From, #state{resources=Resources}=State) ->
    case maps:find(Resource, Resources) of
        {ok, NewSocket} ->
            Flags = gen_server:call(NewSocket, get_flags),
            {reply, {change_socket, NewSocket, Flags}, State};
        error ->
            {reply, close, State}
    end.

handle_cast({attach, Resource, SocketPid}, #state{resources=Resources, monitors=Monitors}=State) ->
    NewResources = Resources#{Resource => SocketPid},

    MonRef = erlang:monitor(process, SocketPid),
    NewMonitors = Monitors#{SocketPid => {Resource, MonRef}},
    {noreply, State#state{resources=NewResources, monitors=NewMonitors}};

handle_cast({detach, Resource}, #state{resources=Resources, monitors=Monitors}=State) ->
    case maps:take(Resource, Resources) of
        {SocketPid, NewResources} ->
            case maps:take(SocketPid, Monitors) of
                {{Resource, MonRef}, NewMonitors} ->
                    erlang:demonitor(MonRef),
                    {noreply, State#state{resources=NewResources, monitors=NewMonitors}};
                _ ->
                    {noreply, State#state{resources=NewResources}}
            end;
        _ ->
            {noreply, State}
    end;

handle_cast(CastMsg, State) ->
    ?LOG_INFO("zmq system error", #{error => unhandled_handle_cast, args => CastMsg }),
    {noreply, State}.

handle_info({'DOWN', MonRef, process, SocketPid, _}, #state{resources=Resources, monitors=Monitors}=State) ->
  case maps:take(SocketPid, Monitors) of
      {{Resource, MonRef}, NewMonitors} ->
          NewResources = maps:remove(Resource, Resources),
          {noreply, State#state{resources=NewResources, monitors=NewMonitors}};
      _ ->
          {noreply, State}
  end;

handle_info({'EXIT', _Pid, {shutdown, invalid_resource}}, State) ->
    {noreply, State};

handle_info(InfoMsg, State) ->
    ?LOG_INFO("zmq system error", #{error => unhandled_handle_info, args => InfoMsg}),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% TODO: close all resources
    ok.
