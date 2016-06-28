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
-module(router_with_dealer).

-export([main/0]).


start_worker(Identity) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = erlangzmq:socket(dealer, Identity),
              {ok, _PeerPid} = erlangzmq:connect(Socket, tcp, "localhost", 5585),
              worker_loop(Socket, Identity, Parent)
      end
     ).

worker_loop(Socket, Identity, Parent) ->
    {ok, Multipart} = erlangzmq:recv_multipart(Socket),
    case Multipart of
        [<<"EXIT">>] ->
            ok;
        _ ->
            Parent ! {recv, Identity, Multipart}
    end.

main() ->
    application:ensure_started(erlangzmq),
    {ok, Socket} = erlangzmq:socket(router),
    {ok, _BindPid} = erlangzmq:bind(Socket, tcp, "localhost", 5585),

    start_worker("A"),
    start_worker("B"),

    timer:sleep(100), %% wait workers to be established

    ok = erlangzmq:send_multipart(Socket, [<<"A">>, <<"My message one">>]),
    ok = erlangzmq:send_multipart(Socket, [<<"B">>, <<"My message two">>]),

    ok = erlangzmq:send_multipart(Socket, [<<"A">>, <<"EXIT">>]),
    ok = erlangzmq:send_multipart(Socket, [<<"B">>, <<"EXIT">>]),

    MessageA = receive
                   {recv, "A", MultipartA} ->
                       MultipartA
               end,

    MessageB = receive
                   {recv, "B", MultipartB} ->
                       MultipartB
               end,
    io:format("Received: ~p...~p\n", [MessageA, MessageB]).
