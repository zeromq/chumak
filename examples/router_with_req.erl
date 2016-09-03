%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(router_with_req).
-export([main/0]).

start_worker(Identity) ->
    Parent = self(),
    spawn_link(
      fun () ->
              {ok, Socket} = chumak:socket(req, Identity),
              {ok, _PeerPid} = chumak:connect(Socket, tcp, "localhost", 5576),
              worker_loop(Socket, Identity, Parent)
      end
     ).

worker_loop(Socket, Identity, Parent) ->
    chumak:send(Socket, <<"ready">>),
    {ok, Message} = chumak:recv(Socket),
    
    io:format("Message received from router ~p\n", [Message]).


main() ->
    application:ensure_started(chumak),
    {ok, Socket} = chumak:socket(router),
    {ok, _BindPid} = chumak:bind(Socket, tcp, "localhost", 5576),

    start_worker("REQ-A"),
    start_worker("REQ-B"),
    loop(Socket).


loop(Socket) ->
    {ok, [Identity, <<>>, <<"ready">>]} = chumak:recv_multipart(Socket),
    ok = chumak:send_multipart(Socket, [Identity, <<>>, <<"Reply">>]),
    loop(Socket).
