%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Peer representation for ZeroMQ protocol

-module(chumak_peer).
-include("chumak.hrl").


-behaviour(gen_server).

%% protocol behaviors
-export([accept/3, accept/2, connect/4, connect/5, connect/6, send/2, send/3, send_error/2,
         send_subscription/2, send_cancel_subscription/2,
         incoming_queue_out/1, reconnect/1, close/1]).
%% gen_server behaviors
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).


-type peer_step() :: waiting_peer | waiting_ready | ready. %% state of connection
-type peer_opts() :: [PeerOpt::peer_opt()].
-type peer_opt()  :: incoming_queue  %% if peer bufferize instead notify parent pid.
                    | multi_socket_type
                    | {curve_server, boolean()}
                    | {curve_publickey, binary()}
                    | {curve_secretkey, binary()}
                    | {curve_serverkey, binary()}.

-type handshake_data() :: {error, term()} |
                          {ready, map()}.
-record(state, {
          step=waiting_ready  :: peer_step(),
          host                :: nil | string(),
          port                :: nil | number(),
          conn_side           :: server | client,
          resource=""         :: string(),
          type                :: socket_type(),
          identity=""         :: string(),  %% identity for this peer
          peer_identity=""    :: string(),  %% identity of remote peet that peer will talking
          peer_version=nil    :: nil | {number(), number()}, %% version of protocol that peer is talking
          socket=nil          :: nil | gen_tcp:socket(),
          decoder=nil         :: nil | chumak_protocol:decoder(),
          parent_pid          :: pid(),
          %% if incoming_queue is used these two properties will be used
          incoming_queue=nil :: nil | queue:queue(),
          msg_buf=[]          :: list(), %% used to bufferize msg until last message found
          %% pub compatible layer is used to wrap the received messages
          pub_compatible_layer=false :: false | true,
          multi_socket_type=false    :: false | true,
          as_server=false     :: boolean(),
          mechanism=null      :: security_mechanism(),
          security_data=#{}   :: chumak_curve:curve_data() | #{}}).

-type state() :: #state{}.

%% @doc connect into a peer
-spec connect(Type::socket_type(),
              Transport::transport(),
              Host::list(),
              Port::integer(),
              Resource::term(),
              Opts::peer_opts()) ->
                     {ok, Pid::pid()} | {error, Reason::term()}.
connect(Type, tcp, Host, Port, Resource, Opts)
  when is_atom(Type),
       is_list(Host),
       is_integer(Port),
       is_list(Resource) ->
    gen_server:start_link(?MODULE, {connect, Type, Host, Port, Resource, Opts, self()}, []).

connect(Type, Protocol, Host, Port) ->
    connect(Type, Protocol, Host, Port, []).

connect(Type, Protocol, Host, Port, Resource) ->
    connect(Type, Protocol, Host, Port, Resource, []).


%% @doc accept new peer from Listen Socket
-spec accept(Type::socket_type() | none, %% none for multi socket type
             Socket::pid(),
             Opts::peer_opts()) ->
                    {ok, Pid::pid()} | {error, Reason::term()}.
accept(Type, Socket, Opts) ->
    gen_server:start_link(?MODULE, {accept, Type, Socket, Opts, self()}, []).

accept(Type, Socket) ->
    accept(Type, Socket, []).

%% @doc send Data to the peer
-spec send(PeerPid::pid(), [Data::binary()], Client::term()) -> ok.
send(PeerPid, Data, Client) ->
    gen_server:cast(PeerPid, {send, Data, Client}).

send(PeerPid, Data) ->
    gen_server:cast(PeerPid, {send, Data}).

send_subscription(PeerPid, Subscription) ->
    gen_server:cast(PeerPid, {send_subscription, Subscription}).

send_cancel_subscription(PeerPid, Subscription) ->
    gen_server:cast(PeerPid, {send_cancel_subscription, Subscription}).

%% @doc when incoming_queue is enabled, get item from queue
-spec incoming_queue_out(PeerPid::pid()) -> {out, Messages::list()} | empty | {error, _}.
incoming_queue_out(PeerPid) ->
    try
        gen_server:call(PeerPid, incoming_queue_out)
    catch
        _Error:Info -> {error,Info}
    end.

%% @doc used to force a peer reconnection, only used for tests
reconnect(PeerPid) ->
    gen_server:cast(PeerPid, reconnect).

%% @doc send error to client
send_error(PeerPid, ReasonMsg) ->
    gen_server:cast(PeerPid, {send_error, ReasonMsg}).

close(PeerPid) ->
    gen_server:stop(PeerPid).

%% gen_server implementation
%% @hidden
%% connect into peer by passing
init({connect, Type, Host, Port, Resource, Opts, ParentPid}) ->
    %% the first job of this gen_server is connect
    gen_server:cast(self(), connect),
    State = pending_connect_state(Type, Host, Port, Resource, Opts, ParentPid),
    {ok, State};

init({accept, Type, SocketPid, Opts, ParentPid}) ->
    {ok, accepted_state(Type, SocketPid, Opts, ParentPid)}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @hidden
handle_call(incoming_queue_out, _From, #state{incoming_queue=nil}=State) ->
    ?LOG_ERROR("zmq queue error", #{error => incoming_queue_not_enabled, queue => incoming_queue_out, type => peer}),
    {reply, {error, incoming_queue_not_enabled}, State};
handle_call(incoming_queue_out, _From, #state{incoming_queue=IncomingQueue}=State) ->
    case queue:out(IncomingQueue) of
        {{value, Messages}, NewQueue} ->
            {reply, {out, Messages}, State#state{incoming_queue=NewQueue}};
        {empty, _IncomingQueue} ->
            {reply, empty, State}
    end.


%% @hidden
handle_cast({send, Multipart, Client}, #state{socket=Socket,
                                              mechanism = Mechanism,
                                              security_data = Security_data,
                                              step=ready}=State) ->
    {Data, NewSecurityData}
       = chumak_protocol:encode_message_multipart(Multipart, Mechanism,
                                                  Security_data),
    case gen_tcp:send(Socket, Data) of
        ok ->
            gen_server:reply(Client, ok);
        {error, Reason}->
            gen_server:reply(Client, {error, Reason})
    end,
    {noreply, State#state{security_data = NewSecurityData}};
handle_cast({send, Multipart}, #state{mechanism = Mechanism,
                                      security_data = SecurityData,
                                      step=ready}=State) ->
    {Data, NewSecurityData} =
        chumak_protocol:encode_message_multipart(Multipart,
                                                 Mechanism,
                                                 SecurityData),
    send_data(Data, State#state{security_data = NewSecurityData});

handle_cast({send_error, ReasonMsg}, #state{socket=Socket}=State) ->
    send_error_to_socket(Socket, ReasonMsg),
    {noreply, State};

handle_cast({send_subscription, Topic}, #state{peer_version={3, 0}}=State) ->
    %% compatibility layer to subscribe in old versions of ZeroMQ
    Frame = chumak_protocol:encode_old_subscribe(Topic),
    send_data(Frame, State);

handle_cast({send_subscription, Topic}, State) ->
    Command = chumak_command:encode_subscribe(Topic),
    Frame = chumak_protocol:encode_command(Command),
    send_data(Frame, State);

handle_cast({send_cancel_subscription, Topic}, #state{peer_version={3, 0}}=State) ->
    %% compatibility layer to unsubscribe in old versions of ZeroMQ
    Frame = chumak_protocol:encode_old_cancel(Topic),
    send_data(Frame, State);

handle_cast({send_cancel_subscription, Topic}, State) ->
    Command = chumak_command:encode_cancel(Topic),
    Frame = chumak_protocol:encode_command(Command),
    send_data(Frame, State);

handle_cast(connect, State) ->
    try_connect(State);

handle_cast(negotiate_greetings, State) ->
    negotiate_greetings(State);

handle_cast(reconnect, #state{socket=Socket}=State) ->
    gen_tcp:close(Socket),
    try_connect(State).

%% @hidden
handle_info({tcp, _Port, Frame}, State) ->
    ok = inet:setopts(State#state.socket, [{active, once}]),
    Reply = chumak_protocol:decode(State#state.decoder, Frame),
    process_decoder_reply(State, Reply);


handle_info({tcp_closed, _Port}, #state{host=nil}=State) ->
    %% when not support reconnect
    {stop, {shutdown, tcp_closed}, State};

handle_info({tcp_closed, _Port}, State) ->
    try_connect(State);

handle_info(InfoMessage, State) ->
    ?LOG_WARNING("zmq system error", #{error => unhandled_handle_info, reason => InfoMessage}),
    {noreply, State}.


%% @hidden
terminate(_Reason, #state{socket=nil}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket),
    ok.

%% private methods
send_data(Data, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ok;
        {error, Reason}->
            ?LOG_ERROR("zmq send error", #{error => send_error, reason => Reason})
    end,
    {noreply, State}.

try_connect(#state{host=Host, port=Port, parent_pid=ParentPid,
                   socket=OldSocketPid, security_data = CurveOptions}=State) ->
    case gen_tcp:connect(Host, Port, ?SOCKET_OPTS([])) of
        {ok, SocketPid} ->
            case OldSocketPid of
                nil ->
                    pass;
                _ ->
                    gen_server:cast(ParentPid, {peer_reconnected, self()}) %% tell chumak_socket we are connected
            end,
            %% update state and start to decode the protocol
            NewState = State#state{
                         socket=SocketPid,
                         decoder=chumak_protocol:new_decoder(CurveOptions)
                        },
            negotiate_greetings(NewState);

        {error, Reason} ->
            ?LOG_ERROR("zmq connection failed", #{error => connection_failed, host => Host, port => Port, reason => Reason}),
            timer:sleep(?RECONNECT_TIMEOUT),
            try_connect(State)
    end.

%% the result must be either:
%% {noreply, NewState#state{decoder=NewDecoder, step=ready}};
%% or:
%% {stop, Error, State}
negotiate_greetings(#state{socket=Socket,
                           mechanism = Mechanism,
                           as_server=AsServer}=State) ->
    try
        %% send and receives greeting bytes
        ok = send_greetting_step(Socket, AsServer, Mechanism), %% send greeting message to another side
        {ok, GreetingFrame} = gen_tcp:recv(Socket, 64, ?GREETINGS_TIMEOUT), %% waiting another side  greeting
        {ready, NewDecoder} = chumak_protocol:decode(State#state.decoder, GreetingFrame),
        verify_mechanism(State, NewDecoder)
    catch
        error:{badmatch, {error, timeout}} ->
            ?LOG_WARNING("zmq handshake timeout", #{error => negotiate_error, reason => timeout}),
            {stop, {shutdown, timeout}, State};
        error:{badmatch, {error, Reason}} ->
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => Reason}),
            {stop, {error, Reason}, State};
        error:{badmatch, Error} ->
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => Error}),
            {stop, Error, State}
    end.

verify_mechanism(#state{mechanism = Mechanism} = State, Decoder) ->
    case chumak_protocol:decoder_mechanism(Decoder) of
        Mechanism ->
            verify_role(State, Decoder);
        _ ->
            MismatchError = {server_error, "Security mechanism mismatch"},
            ?LOG_ERROR("zmq handshake error", #{error => negotiate_error, reason => MismatchError}),
            {stop, {shutdown, MismatchError}, State}
    end.

verify_role(#state{mechanism = curve,
                   as_server = _AsServer} = State, Decoder) ->
    case chumak_protocol:decoder_as_server(Decoder) of
        %% From the interworking tests I can only conclude
        %% that this does not work as I expected.
        %% %% Each peer must have it's own role
        %% AsServer ->
            %% MismatchError = {server_error, "Role (as-server) mismatch"},
            %% ?LOG_ERROR("zmq handshake error", #{error => negotiate_error,
            %%                       reason => MismatchError}]),
            %% {stop, {shutdown, MismatchError}, State};
        _ ->
            do_handshake(State, Decoder)
    end;
verify_role(State, Decoder) ->
    do_handshake(State, Decoder).

do_handshake(#state{socket = Socket} = State, Decoder) ->
    PeerVersion = chumak_protocol:decoder_version(Decoder),
    case handshake(State#state{decoder = Decoder,
                               peer_version = PeerVersion}) of
        {ok, NewState} ->
            %% turn on connection async
            ok = gen_tcp:controlling_process(Socket, self()),
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, NewState};
        {error, Err, NewState} ->
            {stop, Err, NewState}
    end.

-spec handshake(State::state()) ->
          {ok, state()} | {error, Reason::term(), state()}.
%% As described in https://rfc.zeromq.org/spec:26/CURVEZMQ/
handshake(#state{mechanism = curve, socket = Socket,
                 as_server = AsServer, decoder = Decoder,
                 identity = Identity, type = SocketType,
                 resource = Resource} = State) ->
    %% The handshake provides:
    %% - security data:
    %%   - curve_data() in case of curve security,
    %%   - #{} in case of null security
    %% - a "resource" if multi_socket_type == true
    %% - socket type
    %% - identity
    SocketTypeBin = string:to_upper(atom_to_list(SocketType)),
    Metadata = [{"Socket-Type", SocketTypeBin},
                {"Identity", Identity},
                {"Resource", Resource}],
    {NewDecoder, HandshakeResponse}  =
        chumak_curve:security_handshake(Socket, Decoder, AsServer, Metadata),
    handle_handshake_data(State#state{decoder = NewDecoder}, HandshakeResponse);

handshake(#state{mechanism=null, socket = Socket,
                 decoder = Decoder, conn_side = Side} = State) ->
    %% "Note that to avoid deadlocks, each peer MUST send its READY command
    %% before attempting to receive a READY from the other peer. In the NULL
    %% mechanism, peers are symmetric."
    %% But later on in the example this appears to be contradicted. Client
    %% must first send ready.
    case Side of
        client ->
            ok = send_ready_command(State);
        server ->
            ok
    end,
    {ok, IncomingReadyFrame} = recv_ready_command(Socket),
    {ok, NewDecoder, [ReadyCommand]} = chumak_protocol:decode(Decoder,
                                                              IncomingReadyFrame),
    case handle_handshake_data(State#state{decoder=NewDecoder},
                               map_ready_command(ReadyCommand)) of
        {ok, ReadyState} when Side =:= server ->
            ok = send_ready_command(ReadyState),
            {ok, ReadyState};
        Other ->
            Other
    end.

send_ready_command(#state{socket = Socket, resource = Resource,
                          type = Type, identity = Identity}) ->
    ReadyCommand = chumak_command:encode_ready(Type,
                                               Identity,
                                               Resource, #{}),
    send_command_to_socket(Socket, ReadyCommand).

map_ready_command(ReadyCommand) ->
    case chumak_command:command_name(ReadyCommand) of
        ready ->
            {ready,
             #{security_data => #{},
               "resource" => chumak_command:ready_resource(ReadyCommand),
               "socket-type" => chumak_command:ready_socket_type(ReadyCommand),
               "identity" => chumak_command:ready_identity(ReadyCommand)}};
        error ->
            {error, chumak_command:error_reason(ReadyCommand)};
        Name ->
            {error, {invalid_command_before_ready, Name}}
    end.

-spec handle_handshake_data(state(), handshake_data()) ->
          {ok, state()} | {error, Reason::term(), state()}.
handle_handshake_data(State,
                      {error, {invalid_command_before_ready, _Name}} = Error) ->
    {error, Error, State};
handle_handshake_data(State, {error, Reason}) ->
    ?LOG_ERROR("zmq handshake error", #{error => server_error, reason => Reason}),
    {error, {shutdown, {server_error, Reason}}, State};
handle_handshake_data(#state{multi_socket_type=true,
                             parent_pid = ResourceRouterPid} = State,
                      {ready, #{"resource" := Resource} = ReadyData}) ->
    case gen_server:call(ResourceRouterPid, {route_resource, Resource}) of
        {change_socket, NewSocket, {SocketType, Opts}} ->
            NewState = apply_opts(State#state{parent_pid=NewSocket,
                                              type=SocketType}, Opts),
            unlink(ResourceRouterPid),
            link(NewSocket),
            handle_ready_response2(NewState, ReadyData);
        close ->
            send_invalid_resource_error(State#state.socket, Resource),
            {error, {shutdown, invalid_resource}, State}
    end;
handle_handshake_data(State, {ready, ReadyData}) ->
    handle_ready_response2(State, ReadyData).

handle_ready_response2(#state{socket=Socket,
                              type = SocketType} = State,
                       #{"socket-type" := PeerSocketType} = ReadyData) ->
    case validate_peer_socket_type(State, ReadyData) of
        {ok, #state{parent_pid = ParentPid,
                    peer_identity = PeerIdentity} = NewState} ->
            gen_server:cast(ParentPid, {peer_ready, self(), PeerIdentity}),
            {ok, NewState};
        {error, {shutdown, invalid_peer_socket_type}, _} = InvSockTypeError->
            send_invalid_socket_type_error(Socket, SocketType, PeerSocketType),
            InvSockTypeError
    end.

validate_peer_socket_type(#state{type=SocketType} = State,
                          #{"socket-type" := PeerSocketTypeList,
                            security_data := SecurityData} = Metadata) ->
    Identity = maps:get("identity", Metadata, ""),
    PeerSocketType = list_to_atom(string:to_lower(PeerSocketTypeList)),
    PatternModule = chumak_pattern:module(SocketType),
    case PatternModule:valid_peer_type(PeerSocketType) of
        valid ->
            {ok, State#state{step=ready,
                             security_data = SecurityData,
                             peer_identity = Identity}};
        invalid ->
            {error, {shutdown, invalid_peer_socket_type}, State}
    end.


pending_connect_state(Type, Host, Port, Resource, Opts, ParentPid) ->
    State = #state{
               type=Type,
               parent_pid=ParentPid,
               host=Host,
               port=Port,
               resource=Resource,
               conn_side=client
              },
    apply_opts(State, Opts).%% set opts before connect to the server

accepted_state(Type, Socket, Opts, ParentPid) ->
    gen_tcp:controlling_process(Socket, self()),
    State = apply_opts(#state{type=Type,
                              socket=Socket,
                              parent_pid=ParentPid,
                              host=nil,
                              port=nil,
                              conn_side=server}, Opts),
    CurveOptions = State#state.security_data,
    State#state{decoder = chumak_protocol:new_decoder(CurveOptions)}.

apply_opts(State, []) ->
    State;
apply_opts(State, [incoming_queue| Opts]) ->
    IncomingQueue = queue:new(),
    apply_opts(State#state{incoming_queue=IncomingQueue}, Opts);
apply_opts(State, [{identity, Identity}| Opts]) ->
    apply_opts(State#state{identity=Identity}, Opts);
apply_opts(State, [pub_compatible_layer| Opts]) ->
    apply_opts(State#state{pub_compatible_layer=true}, Opts);
apply_opts(State, [multi_socket_type| Opts]) ->
    apply_opts(State#state{multi_socket_type=true}, Opts);
apply_opts(State, [{curve_server, true}| Opts]) ->
    apply_opts(State#state{as_server=true,
                           mechanism=curve}, Opts);
apply_opts(State, [{curve_server, false}| Opts]) ->
    apply_opts(State#state{as_server=false,
                           mechanism=null}, Opts);
apply_opts(State = #state{security_data = CurveOptions}, [{KeyType, Key}| Opts])
    when KeyType == curve_secretkey;
         KeyType == curve_publickey;
         KeyType == curve_clientkeys ->
    apply_opts(State#state{security_data = CurveOptions#{KeyType => Key}},
               Opts);
apply_opts(State = #state{security_data = CurveOptions},
           [{curve_serverkey, Key}| Opts]) ->
    apply_opts(State#state{mechanism=curve,
                           security_data = CurveOptions#{curve_serverkey => Key}},
               Opts).

recv_ready_command(Socket) ->
    {ok, <<4, Size>>} = gen_tcp:recv(Socket, 2, ?GREETINGS_TIMEOUT),
    {ok, Frame} = gen_tcp:recv(Socket, Size, ?GREETINGS_TIMEOUT),
    {ok, <<4, Size, Frame/binary>>}.

process_decoder_reply(State, Reply) ->
    case Reply of
        {ok, Decoder} ->
            {noreply, State#state{decoder=Decoder}};
        {ok, Decoder, Commands} ->
            receive_commands(State, Decoder, Commands);
        {error, Reason} ->
            ?LOG_ERROR("zmq decode error", #{error => decode_fail, reason => Reason}),
            {stop, decode_error, State}
    end.


send_greetting_step(Socket, AsServer, Mechanism) ->
    Greeting = chumak_protocol:build_greeting_frame(AsServer, Mechanism),
    case gen_tcp:send(Socket, Greeting) of
        ok ->
            ok;
        {error, SendReason} ->
            {error, {send_greeting_error, SendReason}}
    end.


receive_commands(#state{step=ready}=State, NewDecoder, []) ->
    {noreply, State#state{decoder=NewDecoder}};

receive_commands(#state{step=ready, parent_pid=ParentPid}=State, NewDecoder, [Command|Commands]) ->
    case chumak_command:command_name(Command) of
        message ->
            NewState = deliver_message(State, Command),
            receive_commands(NewState, NewDecoder, Commands);

        subscribe ->
            Subscription = chumak_command:subscribe_subscription(Command),
            gen_server:cast(ParentPid, {peer_subscribe, self(), Subscription}),
            receive_commands(State, NewDecoder, Commands);

        cancel ->
            Subscription = chumak_command:cancel_subscription(Command),
            gen_server:cast(ParentPid, {peer_cancel_subscribe, self(), Subscription}),
            receive_commands(State, NewDecoder, Commands);

        error ->
            ?LOG_ERROR("zmq receive error", #{error => socket_error, reason => chumak_command:error_reason(Command)}),
            {stop, {shutdown, peer_error}, State};

        Name ->
            {stop, {invalid_command, Name}}
    end.

send_invalid_socket_type_error(Socket, SocketType, PeerSocketType) ->
    ReasonMsg = io_lib:format("Invalid socket-type ~s for ~p server",
                              [PeerSocketType, SocketType]),
    send_error_to_socket(Socket, ReasonMsg).


send_invalid_resource_error(Socket, Resource) ->
    ReasonMsg = io_lib:format("Invalid resource: ~s", [Resource]),
    send_error_to_socket(Socket, ReasonMsg).

send_error_to_socket(Socket, ReasonMsg) ->
    Command = chumak_command:encode_error(ReasonMsg),
    Frame = chumak_protocol:encode_command(Command),

    case gen_tcp:send(Socket, Frame) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("zmq send error", #{error => send_error, reason => Reason})
    end.

send_command_to_socket(Socket, Command) ->
    gen_tcp:send(Socket, chumak_protocol:encode_command(Command)).

deliver_message(#state{peer_version={3, 0}, pub_compatible_layer=true, parent_pid=ParentPid}=State, Message) ->
    Data = chumak_protocol:message_data(Message),

    case Data of
        <<1, SubscribeTopic/binary>> ->
            gen_server:cast(ParentPid, {peer_subscribe, self(), SubscribeTopic});
        <<0, UnsubscribeTopic/binary>> ->
            gen_server:cast(ParentPid, {peer_cancel_subscribe, self(), UnsubscribeTopic})
    end,

    State;

deliver_message(#state{incoming_queue=nil, parent_pid=ParentPid}=State, Message) ->
    %% deliver message directly to parent pid without buffering
    ParentPid ! {peer_recv_message, Message, self()},
    State;

deliver_message(State, Message) ->
    #state{incoming_queue=IncomingQueue, msg_buf=Buffer,
           parent_pid=ParentPid, peer_identity=PeerIdentity} = State,

    NewBuffer = Buffer ++ [chumak_protocol:message_data(Message)],

    case chumak_protocol:message_has_more(Message) of
        %% if need to accumulate more message
        true ->
            State#state{msg_buf=NewBuffer};

        false ->
            NewQueue = queue:in(NewBuffer, IncomingQueue),
            ParentPid ! {queue_ready, PeerIdentity, self()},
            State#state{msg_buf=[], incoming_queue=NewQueue}
    end.
