%% @copyright 2016 Choven Corp.
%%
%% This file is part of chumak.
%%
%% chumak is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% chumak is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with chumak.  If not, see <http://www.gnu.org/licenses/>

-module(chumak_acceptance_pair).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5587).

normal_test_() ->
    [
    {
       "Should deliver message for the paired peer",
       {setup, fun start/0, fun stop/1, fun negotiate_without_multipart/1}
     }
    , {
       "Should deliver message for the paired peer using multipart",
       {setup, fun start/0, fun stop/1, fun negotiate_with_multipart/1}
     }
    , {
       "Should deliver message for the paired peer without timeout",
       {setup, fun start/0, fun stop/1, fun negotiate_without_timeout/1}
     }
     , {
       "Should deliver message for the paired peer with delayed message",
       {setup, fun start/0, fun stop/1, fun negotiate_with_delay/1}
     }
    ].

start() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(pair),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", ?PORT),
    Socket.

start_worker(Identity,  Func) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(pair, Identity),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", ?PORT),
              Func(Socket, Identity, Parent)
      end
     ).

stop(Pid) ->
    gen_server:stop(Pid).

negotiate_without_multipart(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-A", NegociateFunc),
    timer:sleep(200),
    ok = chumak:send(Socket, <<"Hey brother">>),

    Message = receive
        {recv, "PAIR-A", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, <<"Hey brother">>)
    ].

negotiate_with_multipart(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv_multipart(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-B", NegociateFunc),
    timer:sleep(200),
    ok = chumak:send_multipart(Socket, [<<"Hey">>, <<"Jude">>]),

    Message = receive
        {recv, "PAIR-B", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, [<<"Hey">>, <<"Jude">>])
    ].

negotiate_without_timeout(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-C", NegociateFunc),
    ok = chumak:send(Socket, <<"Hey mother">>),

    Message = receive
        {recv, "PAIR-C", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, <<"Hey mother">>)
    ].

negotiate_with_delay(Socket) ->
    NegociateFunc = fun (ClientSocket, Identity, Parent) ->
                            timer:sleep(200),
                            {ok, Message} = chumak:recv(ClientSocket),
                            Parent ! {recv, Identity, Message}
                    end,
    start_worker("PAIR-D", NegociateFunc),
    ok = chumak:send(Socket, <<"Hey father">>),

    Message = receive
        {recv, "PAIR-D", MultipartA} ->
            MultipartA
    end,
    [
     ?_assertEqual(Message, <<"Hey father">>)
    ].
