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

%% @doc ZeroMQ Listener for new connections.
-module(erlangzmq_bind).
-include("erlangzmq.hrl").

-export([start_link/2, listener/2]).

-spec start_link(Host::string(), Port::number()) -> {ok, BindPid::pid()} | {error, Reason::term()}.
start_link(Host, Port) ->
    ParentPid = self(),

    case inet:getaddr(Host, inet) of
        {ok, Addr} ->
            case gen_tcp:listen(Port, ?SOCKET_OPTS([{ip, Addr}])) of
                {ok, ListenSocket} ->
                    Pid = spawn_link(?MODULE, listener, [ListenSocket, ParentPid]),
                    {ok, Pid};
                {error, Reason} ->
                    error_logger:error_report([
                                               bind_error,
                                               {host, Host},
                                               {addr, Addr},
                                               {port, Port},
                                               listen_error,
                                               {error, Reason}
                                              ]),
                    {error, Reason}
            end;

        {error, IpReason} ->
            error_logger:error_report([
                                       bind_error,
                                       {host, Host},
                                       getaddr_error,
                                       {error, IpReason}
                                      ]),
            {error, IpReason}
    end.


listener(ListenSocket, ParentPid) ->
    try
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        {ok, PeerPid} = gen_server:call(ParentPid, {accept, Socket}),
        ok = gen_tcp:controlling_process(Socket, PeerPid),
        %% Start to negociate greetings after new process is owner
        gen_server:cast(PeerPid, negotiate_greetings),
        listener(ListenSocket, ParentPid)
    catch
        error:{badmatch, {error, closed}} ->
            error_logger:info_report({bind_closed});
        error:{badmatch, Error} ->
            error_logger:error_report([
                                       accept_error,
                                       {error, Error}
                                      ]),
            listener(ListenSocket, ParentPid)
    end.
