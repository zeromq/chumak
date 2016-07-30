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

%% @doc Peer representation for ZeroMQ protocol

-module(erlangzmq_peer).
-include("erlangzmq.hrl").


-behaviour(gen_server).

%% protocol behaviors
-export([accept/3, accept/2, connect/4, connect/5, connect/6, send/2, send/3, send_error/2,
         send_subscription/2, send_cancel_subscription/2,
         incomming_queue_out/1, reconnect/1, close/1]).
%% gen_server behaviors
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).


-type peer_step() :: waiting_peer | waiting_ready | ready. %% state of connection
-type peer_opts() :: [PeerOpt::peer_opt()].
-type peer_opt()  :: incomming_queue  %% if peer bufferize instead notify parent pid.
                    | multi_socket_type. 

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
          decoder=nil         :: nil | erlangzmq_protocol:decoder(),
          parent_pid          :: pid(),
          %% if incomming_queue is used these two properties will be used
          incomming_queue=nil :: nil | queue:queue(),
          msg_buf=[]          :: list(), %% used to bufferize msg until last message found
          %% pub compatible layer is used to wrap the received messages
          pub_compatible_layer=false :: false | true,
          multi_socket_type=false    :: false | true
         }).

%% @doc connect into a peer
-spec connect(Type::socket_type(),
              Transport::transport(),
              Host::list(),
              Port::integer(),
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
-spec send(PeerPid::pid(), Data::binary(), Client::term()) -> ok.
send(PeerPid, Data, Client) ->
    gen_server:cast(PeerPid, {send, Data, Client}).

send(PeerPid, Data) ->
    gen_server:cast(PeerPid, {send, Data}).

send_subscription(PeerPid, Subscription) ->
    gen_server:cast(PeerPid, {send_subscription, Subscription}).

send_cancel_subscription(PeerPid, Subscription) ->
    gen_server:cast(PeerPid, {send_cancel_subscription, Subscription}).

%% @doc when incomming_queue is enabled, get item from queue
-spec incomming_queue_out(PeerPid::pid()) -> {out, Messages::list()} | empty.
incomming_queue_out(PeerPid) ->
    gen_server:call(PeerPid, incomming_queue_out).

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
handle_call(incomming_queue_out, _From, #state{incomming_queue=nil}=State) ->
    error_logger:error_report([
                               incomming_queue_out,
                               {error, incomming_queue_not_enabled}
                              ]),
    {reply, {error, incomming_queue_not_enabled}, State};
handle_call(incomming_queue_out, _From, #state{incomming_queue=IncommingQueue}=State) ->
    case queue:out(IncommingQueue) of
        {{value, Messages}, NewQueue} ->
            {reply, {out, Messages}, State#state{incomming_queue=NewQueue}};
        {empty, _IncommingQueue} ->
            {reply, empty, State}
    end.


%% @hidden
handle_cast({send, Data, Client}, #state{socket=Socket, step=ready}=State) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            gen_server:reply(Client, ok);
        {error, Reason}->
            gen_server:reply(Client, {error, Reason})

    end,
    {noreply, State};
handle_cast({send, Data}, #state{socket=Socket, step=ready}=State) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ok;
        {error, Reason}->
            error_logger:warning_report([
                                         send_error,
                                         {error, Reason}
                                        ])
    end,
    {noreply, State};

handle_cast({send_error, ReasonMsg}, #state{socket=Socket}=State) ->
    send_error_to_socket(Socket, ReasonMsg),
    {noreply, State};

handle_cast({send_subscription, Topic}, #state{peer_version={3, 0}}=State) ->
    %% compatibility layer to subscribe in old versions of ZeroMQ
    Frame = erlangzmq_protocol:encode_old_subscribe(Topic),
    handle_cast({send, Frame}, State);

handle_cast({send_subscription, Topic}, State) ->
    Command = erlangzmq_command:encode_subscribe(Topic),
    Frame = erlangzmq_protocol:encode_command(Command),
    handle_cast({send, Frame}, State);

handle_cast({send_cancel_subscription, Topic}, #state{peer_version={3, 0}}=State) ->
    %% compatibility layer to unsubscribe in old versions of ZeroMQ
    Frame = erlangzmq_protocol:encode_old_cancel(Topic),
    handle_cast({send, Frame}, State);

handle_cast({send_cancel_subscription, Topic}, State) ->
    Command = erlangzmq_command:encode_cancel(Topic),
    Frame = erlangzmq_protocol:encode_command(Command),
    handle_cast({send, Frame}, State);

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
    Reply = erlangzmq_protocol:decode(State#state.decoder, Frame),
    process_decoder_reply(State, Reply);


handle_info({tcp_closed, _Port}, #state{host=nil}=State) ->
    %% when not support reconnect
    {stop, {shutdown, tcp_closed}, State};

handle_info({tcp_closed, _Port}, State) ->
    try_connect(State);

handle_info(InfoMessage, State) ->
    error_logger:info_report([
                              unhandled_handle_info,
                              {msg, InfoMessage}
                             ]),
    {noreply, State}.


%% @hidden
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket),
    ok.

%% private methods
try_connect(#state{host=Host, port=Port, parent_pid=ParentPid, socket=OldSocketPid}=State) ->
    case gen_tcp:connect(Host, Port, ?SOCKET_OPTS([])) of
        {ok, SocketPid} ->
            case OldSocketPid of
                nil ->
                    pass;
                _ ->
                    gen_server:cast(ParentPid, {peer_reconnected, self()})
            end,

            NewState = State#state{
                         socket=SocketPid,
                         decoder=erlangzmq_protocol:new_decoder()
                        },
            negotiate_greetings(NewState);

        {error, Reason} ->
            error_logger:error_report([
                                       {host, Host},
                                       {port, Port},
                                       connection_error,
                                       {error, Reason}
                                      ]),
            timer:sleep(?RECONNECT_TIMEOUT),
            try_connect(State)
    end.

negotiate_greetings(#state{socket=Socket}=State) ->
    try
        %% send and receives greeating bytes
        ok = send_greetting_step(Socket),
        {ok, GreetingFrame} = gen_tcp:recv(Socket, 64, ?GREETINGS_TIMEOUT),
        {ready, NewDecoder} = erlangzmq_protocol:decode(State#state.decoder, GreetingFrame),

        %% send and receives ready command
        negotiate_ready_command(State#state{decoder=NewDecoder})

    catch
        error:{badmatch, Error} ->
            error_logger:error_report([
                                       negotiate_greetings_error,
                                       {error, Error}
                                      ]),
            {stop, Error, State}
    end.

negotiate_ready_command(#state{socket=Socket, multi_socket_type=true}=State) ->
    {ok, IncommingReadyFrame} = recv_ready_command(Socket),
    {ok, NewDecoder, [ReadyCommand]} = erlangzmq_protocol:decode(State#state.decoder, IncommingReadyFrame),

    Resource = erlangzmq_command:ready_resource(ReadyCommand),
    ResourceRouterPid = State#state.parent_pid,

    case gen_server:call(ResourceRouterPid, {route_resource, Resource}) of
        {change_socket, NewSocket, {SocketType, Opts}} ->
            NewState = apply_opts(State#state{parent_pid=NewSocket, type=SocketType, decoder=NewDecoder}, Opts),
            OutcommingReadyCommand = erlangzmq_command:encode_ready(SocketType, NewState#state.identity, "", #{}),

            unlink(ResourceRouterPid),
            link(NewSocket),

            ok = send_command_to_socket(NewState#state.socket, OutcommingReadyCommand),
            turn_async_mode(NewState, ReadyCommand);

        close ->
            send_invalid_resource_error(State#state.socket, Resource),
            {stop, {shutdown, invalid_resource}, State}
    end;

negotiate_ready_command(#state{socket=Socket, resource=Resource, conn_side=server}=State) ->
    %% when connection is in server mode, we need to receive ready command first, after that send the command ready.
    {ok, IncommingReadyFrame} = recv_ready_command(Socket),
    {ok, NewDecoder, [ReadyCommand]} = erlangzmq_protocol:decode(State#state.decoder, IncommingReadyFrame),

    case turn_async_mode(State#state{decoder=NewDecoder}, ReadyCommand) of
        {noreply, NewState} ->
            OutcommingReadyCommand = erlangzmq_command:encode_ready(NewState#state.type, NewState#state.identity, Resource, #{}),
            ok = send_command_to_socket(Socket, OutcommingReadyCommand),

            {noreply, NewState};
        X ->
            X  %% only repass when error is found
    end;

negotiate_ready_command(#state{socket=Socket, resource=Resource, conn_side=client}=State) ->
    OutcommingReadyCommand = erlangzmq_command:encode_ready(State#state.type, State#state.identity, Resource, #{}),
    ok = send_command_to_socket(State#state.socket, OutcommingReadyCommand),
    {ok, IncommingReadyFrame} = recv_ready_command(Socket),
    {ok, NewDecoder, [ReadyCommand]} = erlangzmq_protocol:decode(State#state.decoder, IncommingReadyFrame),
    turn_async_mode(State#state{decoder=NewDecoder}, ReadyCommand).

turn_async_mode(#state{socket=Socket, decoder=Decoder}=State, ReadyCommand) ->
    %% turn on connection async
    ok = gen_tcp:controlling_process(Socket, self()),
    ok = inet:setopts(Socket, [{active, once}]),

    %% recv the version from the peer
    PeerVersion = erlangzmq_protocol:decoder_version(Decoder),
    validate_ready_command(State#state{peer_version=PeerVersion}, ReadyCommand).

pending_connect_state(Type, Host, Port, Resource, Opts, ParentPid) ->
    State = #state{
               type=Type,
               parent_pid=ParentPid,
               host=Host,
               port=Port,
               resource=Resource,
               conn_side=client
              },
    apply_opts(State, Opts).

accepted_state(Type, Socket, Opts, ParentPid) ->
    gen_tcp:controlling_process(Socket, self()),
    State = #state{
               type=Type,
               socket=Socket,
               decoder=erlangzmq_protocol:new_decoder(),
               parent_pid=ParentPid,
               host=nil,
               port=nil,
               conn_side=server
              },
    apply_opts(State, Opts).


apply_opts(State, []) ->
    State;
apply_opts(State, [incomming_queue| Opts]) ->
    IncommingQueue = queue:new(),
    apply_opts(State#state{incomming_queue=IncommingQueue}, Opts);
apply_opts(State, [{identity, Identity}| Opts]) ->
    apply_opts(State#state{identity=Identity}, Opts);
apply_opts(State, [pub_compatible_layer| Opts]) ->
    apply_opts(State#state{pub_compatible_layer=true}, Opts);
apply_opts(State, [multi_socket_type| Opts]) ->
    apply_opts(State#state{multi_socket_type=true}, Opts).

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
            error_logger:error_report([
                                       decode_fail,
                                       {reason, Reason}
                                      ]),
            {stop, decode_error, State}
    end.


send_greetting_step(Socket) ->
    Greeting = erlangzmq_protocol:build_greeting_frame(),
    case gen_tcp:send(Socket, Greeting) of
        ok ->
            ok;
        {error, SendReason} ->
            {error, {send_greeting_error, SendReason}}
    end.


receive_commands(#state{step=ready}=State, NewDecoder, []) ->
    {noreply, State#state{decoder=NewDecoder}};

receive_commands(#state{step=ready, parent_pid=ParentPid}=State, NewDecoder, [Command|Commands]) ->
    case erlangzmq_command:command_name(Command) of
        message ->
            NewState = deliver_message(State, Command),
            receive_commands(NewState, NewDecoder, Commands);

        subscribe ->
            Subscription = erlangzmq_command:subscribe_subscription(Command),
            gen_server:cast(ParentPid, {peer_subscribe, self(), Subscription}),
            receive_commands(State, NewDecoder, Commands);

        cancel ->
            Subscription = erlangzmq_command:cancel_subscription(Command),
            gen_server:cast(ParentPid, {peer_cancel_subscribe, self(), Subscription}),
            receive_commands(State, NewDecoder, Commands);

        error ->
            error_logger:error_report([
                                       socket_error,
                                       {reason, erlangzmq_command:error_reason(Command)}
                                      ]),
            {stop, {shutdown, peer_error}, State};

        Name ->
            {stop, {invalid_command, Name}}
    end.

validate_ready_command(#state{decoder=Decoder}=State, ReadyCommand) ->
    case erlangzmq_command:command_name(ReadyCommand) of
        ready ->
            validate_peer_socket_type(State, ReadyCommand, Decoder);
        error ->
            Reason = erlangzmq_command:error_reason(ReadyCommand),
            error_logger:error_report([
                                       server_error,
                                       {msg, Reason}
                                      ]),
            {stop, {shutdown, {server_error, Reason}}, State};
        Name ->
            {stop, {invalid_command_before_ready, Name}, State}
    end.


validate_peer_socket_type(State, ReadyCommand, NewDecoder) ->
    #state{type=SocketType, socket=Socket, parent_pid=ParentPid}=State,

    PatternModule = erlangzmq_pattern:module(SocketType),
    PeerSocketType = erlangzmq_command:ready_socket_type(ReadyCommand),
    Identity = erlangzmq_command:ready_identity(ReadyCommand),
    NewState = State#state{peer_identity=Identity},

    case PatternModule:valid_peer_type(PeerSocketType) of
        valid ->
            gen_server:cast(ParentPid, {peer_ready, self(), Identity}),
            {noreply, NewState#state{decoder=NewDecoder, step=ready}};
        invalid ->
            send_invalid_socket_type_error(Socket, SocketType, PeerSocketType),
            {stop, {shutdown, invalid_peer_socket_type}, NewState}
    end.

send_invalid_socket_type_error(Socket, SocketType, PeerSocketType) ->
    ReasonMsg = io_lib:format("Invalid socket-type ~s for ~p server", [PeerSocketType, SocketType]),
    send_error_to_socket(Socket, ReasonMsg).


send_invalid_resource_error(Socket, Resource) ->
    ReasonMsg = io_lib:format("Invalid resource: ~s", [Resource]),
    send_error_to_socket(Socket, ReasonMsg).

send_error_to_socket(Socket, ReasonMsg) ->
    Command = erlangzmq_command:encode_error(ReasonMsg),
    Frame = erlangzmq_protocol:encode_command(Command),

    case gen_tcp:send(Socket, Frame) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("Error sending socket error: ~p\n", [Reason])
    end.

send_command_to_socket(Socket, Command) ->
    gen_tcp:send(Socket, erlangzmq_protocol:encode_command(Command)).

deliver_message(#state{peer_version={3, 0}, pub_compatible_layer=true, parent_pid=ParentPid}=State, Message) ->
    Data = erlangzmq_protocol:message_data(Message),

    case Data of
        <<1, SubscribeTopic/binary>> ->
            gen_server:cast(ParentPid, {peer_subscribe, self(), SubscribeTopic});
        <<0, UnsubscribeTopic/binary>> ->
            gen_server:cast(ParentPid, {peer_cancel_subscribe, self(), UnsubscribeTopic})
    end,

    State;

deliver_message(#state{incomming_queue=nil, parent_pid=ParentPid}=State, Message) ->
    %% deliver message directly to parent pid without buffering
    ParentPid ! {peer_recv_message, Message, self()},
    State;

deliver_message(State, Message) ->
    #state{incomming_queue=IncommingQueue, msg_buf=Buffer,
           parent_pid=ParentPid, peer_identity=PeerIdentity} = State,

    NewBuffer = Buffer ++ [erlangzmq_protocol:message_data(Message)],

    case erlangzmq_protocol:message_has_more(Message) of
        %% if need to accumulate more message
        true ->
            State#state{msg_buf=NewBuffer};

        false ->
            NewQueue = queue:in(NewBuffer, IncommingQueue),
            ParentPid ! {queue_ready, PeerIdentity, self()},
            State#state{msg_buf=[], incomming_queue=NewQueue}
    end.
