%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(router_with_dealer).

-export([main/0]).


start_worker(Identity) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(dealer, Identity),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", 5585),
              worker_loop(Socket, Identity, Parent)
      end
     ).

worker_loop(Socket, Identity, Parent) ->
    {ok, Multipart} = chumak:recv_multipart(Socket),
    case Multipart of
        [<<"EXIT">>] ->
            ok;
        _ ->
            Parent ! {recv, Identity, Multipart}
    end.

main() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(router),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", 5585),

    start_worker("A"),
    start_worker("B"),

    timer:sleep(100), %% wait workers to be established

    ok = chumak:send_multipart(Socket, [<<"A">>, <<"My message one">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"My message two">>]),

    ok = chumak:send_multipart(Socket, [<<"A">>, <<"EXIT">>]),
    ok = chumak:send_multipart(Socket, [<<"B">>, <<"EXIT">>]),

    MessageA = receive
                   {recv, "A", MultipartA} ->
                       MultipartA
               end,

    MessageB = receive
                   {recv, "B", MultipartB} ->
                       MultipartB
               end,
    io:format("Received: ~p...~p\n", [MessageA, MessageB]).
