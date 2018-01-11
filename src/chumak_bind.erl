%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Listener for new connections.
-module(chumak_bind).
-include("chumak.hrl").

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
        {ok, PeerPid} = gen_server:call(ParentPid, {accept, Socket}), %% get peer's pid of chumak_peer
        ok = gen_tcp:controlling_process(Socket, PeerPid), %% set controlling of new socket to chumak_peer
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
