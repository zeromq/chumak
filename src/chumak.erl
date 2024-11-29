%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Erlang bindings for ZeroMQ.
%%
%% see ZeroMQ 3.1 RFC in http://rfc.zeromq.org/spec:37

-module(chumak).
-include("chumak.hrl").
-behaviour(application).

-export([start/2, stop/1]).
-export([socket/1, socket/2, connect/4, connect/5, bind/4, send/2, recv/1, send_multipart/2, recv_multipart/1,
         unblock/1,
         set_socket_option/3,
         cancel/2, subscribe/2,
         resource/0, attach_resource/3, detach_resource/2,
         version/0]).

-define(SUPERVISOR, chumak_sup).

-type version()::{Major::integer(), Minor::integer(), Patch::integer()}.

-export_type([security_mechanism/0]).

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
-spec socket(Type::socket_type() | atom(), Identity::string()) ->
                    {ok, SocketPid::pid()} | {error, Reason::atom()}.
socket(Type, Identity)
  when is_atom(Type),
       is_list(Identity) ->
    ?SUPERVISOR:start_socket(Type, Identity).

socket(Type)
  when is_atom(Type) ->
    ?SUPERVISOR:start_socket(Type).

%% @doc set socket option.
%% In case of a problem an error is returned and the socket remains unchanged.
%%
%% Valid options are:
%% <dl>
%%   <dt>curve_server</dt>
%%   <dd>- Set CURVE server role</dd>
%%   <dd>- type: boolean()</dd>
%%   <dd>Defines whether the socket will act as server for CURVE security. A
%%   value of true means the socket will act as CURVE server. A value of false
%%   means the socket will not act as CURVE server. When you set this you must
%%   also set the server's secret key using the curve_secretkey option. A server
%%   socket does not need to know its own public key.</dd>
%%
%%   <dt>curve_serverkey</dt>
%%   <dd>- Set CURVE server key</dd>
%%   <dd>- type: binary() or string()</dd>
%%   <dd>Sets the socket's long term server key. You must set this on CURVE
%%   client sockets. You can provide the key as 32 binary bytes, or as a
%%   40-character string encoded in the Z85 encoding format. This key must have
%%   been generated together with the server's secret key.</dd>
%%
%%   <dt>curve_secretkey</dt>
%%   <dd>- Set CURVE secret key</dd>
%%   <dd>- type: binary() or string()</dd>
%%   <dd>Sets the socket's long term secret key. You must set this on both
%%   CURVE client and server sockets. You can provide the key as 32 binary
%%   bytes, or as a 40-character string encoded in the Z85 encoding format.</dd>
%%
%%   <dt>curve_publickey</dt>
%%   <dd>- Set CURVE public key</dd>
%%   <dd>- type: binary() or string()</dd>
%%   <dd>Sets the socket's long term public key. You must set this on CURVE
%%   client sockets. You can provide the key as 32 binary bytes, or as a
%%   40-character string encoded in the Z85 encoding format. The public key
%%   must always be used with the matching secret key.</dd>
%%
%%   <dt>curve_clientkeys</dt>
%%   <dd>- Provide public keys of authorised clients</dd>
%%   <dd>- type: list of string() or integer() , or 'any'</dd>
%%   <dd>Determines which clients are authorised to connect to the server. You
%%   may set this on CURVE server sockets. If it is not set explicitly, if
%%   defaults to 'any', which means that all clients have access (provided that
%%   they know the CURVE secret key). If a list of keys is provided, only
%%   clients with those public keys can connect to the server. You can provide
%%   the keys as 32-byte binaries or as 40-character strings.</dd>
%% </dl>

-spec set_socket_option(SocketPid::pid(),
                        Option::socket_option(), Value::term()) ->
    ok | {error, Reason::atom()}.
set_socket_option(SocketPid, Option, Value)
  when is_pid(SocketPid),
       is_atom(Option) ->
    gen_server:call(SocketPid, {set_option, Option, Value}).

%% @doc socket to a peer
-spec connect(SocketPid::pid(), Transport::transport(),
              Host::string(), Port::integer(), Resource::term()) ->
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
-spec bind(SocketPid::pid(), Transport::transport(), Host::string(), Port::integer()) ->
    {ok, pid()} | {error, term()}.

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
-spec recv_multipart(SocketPid::pid()) -> {ok, [Data::binary()]} | {error, Reason::atom()}.
recv_multipart(SocketPid)
  when is_pid(SocketPid) ->
    gen_server:call(SocketPid, recv_multipart, infinity).


%% @doc unblock a socket that is waiting on a message,
%% return {error, again}
-spec unblock(SocketPid::pid()) -> ok.
unblock(SocketPid)
  when is_pid(SocketPid) ->
    gen_server:call(SocketPid, unblock).

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

%% @doc detach socket resource from resource server.
-spec detach_resource(ResourcePid::pid(), Resource::binary()) -> ok.
detach_resource(ResourcePid, Resource) ->
    gen_server:cast(ResourcePid, {detach, Resource}).


-spec version() -> {ok, Version::version()} | {error, Reason::atom()}.
version() ->
  case application:get_application(chumak) of
    {ok, chumak} ->
      {ok, return_version()};
    undefined -> {error, application_not_started}
  end.

return_version() ->
  {ok, Version} = application:get_key(chumak, vsn),
  [X, Y, Z] = string:tokens(Version, "."),
  Major = list_to_integer(X),
  Minor = list_to_integer(Y),
  Patch = list_to_integer(Z),
  {Major, Minor, Patch}.
