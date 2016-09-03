%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_sup).
-behaviour(supervisor).

-include("chumak.hrl").

-define(SUPERVISOR_FLAGS, #{strategy => one_for_one}).
-define(CHILD_PROCESS_PREFIX, "chumak_socket_").
-define(SOCKET, chumak_socket).
-define(RESOURCE, chumak_resource).


-export([start_link/0, init/1]).
-export([start_socket/1, start_socket/2, start_resource/0]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    {ok, {?SUPERVISOR_FLAGS, []}}.

-spec start_socket(Type::socket_type(), Identity::string()) -> {ok, SocketPid::pid()} | {error, Reason::atom()}.
start_socket(Type, Identity) ->
    ProcessId = list_to_atom(string:concat(?CHILD_PROCESS_PREFIX, Identity)),
    supervisor:start_child(?MODULE, #{
                             id=>ProcessId,
                             start=>{?SOCKET, start_link, [Type, Identity]}
                            }).

start_socket(Type) ->
    %% socket without identity not use supervisor because the identity
    %% is used to localize process inside supervisor.
    ?SOCKET:start_link(Type, "").

start_resource() ->
    %% Resource not use supervisor yet, because it's needed a identifier
    ?RESOURCE:start_link().
