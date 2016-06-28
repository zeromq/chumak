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

%% @doc Erlang bindings for ZeroMQ.
%%
%% see ZeroMQ 3.1 RFC in http://rfc.zeromq.org/spec:37

-module(erlangzmq).
-include("erlangzmq.hrl").
-behaviour(application).

-export([start/2, stop/1]).
-export([socket/1, socket/2, connect/4, connect/5, bind/4, send/2, recv/1, send_multipart/2, recv_multipart/1,
         cancel/2, subscribe/2,
         resource/0, attach_resource/3]).

-define(SUPERVISOR, erlangzmq_sup).

%%
%% OTP/Application behaviour
%%
%% @hidden
start(_StartType, _StartArgs) ->
    ?SUPERVISOR:start_link().


%% @hidden
stop(_State) ->
    ok.


%% @doc start a new socket
-spec socket(Type::socket_type(), Identity::string()) ->
                    {ok, SocketPid::pid()} | {error, Reason::atom()}.
socket(Type, Identity)
  when is_atom(Type),
       is_list(Identity) ->
    ?SUPERVISOR:start_socket(Type, Identity).

socket(Type)
  when is_atom(Type) ->
    ?SUPERVISOR:start_socket(Type).


%% @doc socket to a peer
-spec connect(SocketPid::pid(), Transport::transport(), Host::string(), Port::integer()) ->
                     {ok, PeerPid::pid()} | {error, Reason::atom()}.
connect(SocketPid, Transport, Host, Port, Resource)
  when is_pid(SocketPid),
       is_atom(Transport),
       is_list(Host),
       is_number(Port),
       is_list(Resource)->

    gen_server:call(SocketPid, {connect, Transport, Host, Port, Resource}).

connect(SocketPid, Transport, Host, Port) ->
    connect(SocketPid, Transport, Host, Port, "").

%% @doc bind in a host and port
-spec bind(SocketPid::pid(), Transport::transport(), Host::string(), Port::integer()) -> ok.
bind(SocketPid, Transport, Host, Port)
  when is_pid(SocketPid),
       is_atom(Transport),
       is_list(Host),
       is_number(Port)->

    gen_server:call(SocketPid, {bind, Transport, Host, Port}).


%% @doc send a message for peers
-spec send(SocketPid::pid(), Data::binary()) -> ok.
send(SocketPid, Data)
  when is_pid(SocketPid),
       is_binary(Data) ->

    gen_server:call(SocketPid, {send, Data}, infinity);

send(SocketPid, Data)
  when is_pid(SocketPid),
       is_list(Data) ->
    send(SocketPid, list_to_binary(Data)).

%% @doc send a message for peers using a list of binaries
-spec send_multipart(SocketPid::pid(), [Data::binary()]) -> ok.
send_multipart(SocketPid, Multipart)
  when is_pid(SocketPid),
       is_list(Multipart) ->

    gen_server:call(SocketPid, {send_multipart, Multipart}, infinity).

%% @doc recv a message for peers
-spec recv(SocketPid::pid()) -> {ok, Data::binary()} | {error, Reason::atom()}.
recv(SocketPid)
  when is_pid(SocketPid) ->
    gen_server:call(SocketPid, recv, infinity).


%% @doc recv a message for peers by a list of binaries
-spec recv_multipart(SocketPid::pid()) -> {ok, [Data::binary]} | {error, Reason::atom()}.
recv_multipart(SocketPid)
  when is_pid(SocketPid) ->
    gen_server:call(SocketPid, recv_multipart, infinity).


%% @doc subscribe a topic, only supported in SUB and XSUB patterns.
-spec subscribe(SocketPid::pid(), Topic::binary()) -> ok.

subscribe(SocketPid, Topic)
  when is_pid(SocketPid),
       is_binary(Topic) ->
    gen_server:cast(SocketPid, {subscribe, Topic});
subscribe(SocketPid, Topic)
  when is_pid(SocketPid),
       is_list(Topic) ->
    subscribe(SocketPid, list_to_binary(Topic)).

%% @doc cancel a subscription for a topic, only supported in SUB and XSUB patterns.
-spec cancel(SocketPid::pid(), Topic::binary()) -> ok.
cancel(SocketPid, Topic)
  when is_pid(SocketPid),
       is_binary(Topic) ->
    gen_server:cast(SocketPid, {cancel, Topic});
cancel(SocketPid, Topic)
  when is_pid(SocketPid),
       is_list(Topic) ->
    cancel(SocketPid, list_to_binary(Topic)).

%% @doc start a new resource server.
-spec resource() -> {ok, ResourcePid::pid()} | {error, Reason::atom()}.
resource() ->
    ?SUPERVISOR:start_resource().

%% @doc attach a new socket as a resource into resource server.
-spec attach_resource(ResourcePid::pid(), Resource::binary(), SocketPid::pid()) -> ok.
attach_resource(ResourcePid, Resource, SocketPid) ->
    gen_server:cast(ResourcePid, {attach, Resource, SocketPid}).
