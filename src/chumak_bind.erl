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

    case getaddr(Host) of
        {ok, Addr} ->
            case gen_tcp:listen(Port, ?SOCKET_OPTS([{ip, Addr}])) of
                {ok, ListenSocket} ->
                    Pid = spawn_link(?MODULE, listener, [ListenSocket, ParentPid]),
                    {ok, Pid};
                {error, Reason} ->
                    ?LOG_ERROR("zmq listen error", #{error => listen_error, host => Host, addr => Addr, port => Port, reason => Reason}),
                    {error, Reason}
            end;

        {error, IpReason} ->
            ?LOG_ERROR("zmq listen error", #{error => getaddr_error, host => Host, reason => IpReason}),
            {error, IpReason}
    end.


listener(ListenSocket, ParentPid) ->
    try
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        {ok, PeerPid} = gen_server:call(ParentPid, {accept, Socket}), %% get peer's pid of chumak_peer
        ok = gen_tcp:controlling_process(Socket, PeerPid), %% set controlling of new socket to chumak_peer
        %% Start to negotiate greetings after new process is owner
        gen_server:cast(PeerPid, negotiate_greetings),
        listener(ListenSocket, ParentPid)
    catch
        error:{badmatch, {error, closed}} ->
            ?LOG_INFO("zmq listener error", #{error => bind_closed});
        error:{badmatch, {error, Reason}} ->
            ?LOG_ERROR("zmq listener error", #{error => accept_error, reason => Reason }),
            listener(ListenSocket, ParentPid);
        error:{badmatch, Error} ->
            ?LOG_ERROR("zmq listener error", #{error => accept_error, reason => Error }),
            listener(ListenSocket, ParentPid)
    end.

-spec getaddr(Host::string()) -> {ok, inet:ip_address() | any} | {error, Reason::term()}.

getaddr("*") ->
    {ok, any};

getaddr(Host) ->
    inet:getaddr(Host, inet).
