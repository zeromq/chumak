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

-module(erlangzmq_sup).
-behaviour(supervisor).

-include("erlangzmq.hrl").

-define(SUPERVISOR_FLAGS, #{strategy => one_for_one}).
-define(CHILD_PROCESS_PREFIX, "erlangzmq_socket_").
-define(SOCKET, erlangzmq_socket).
-define(RESOURCE, erlangzmq_resource).


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
