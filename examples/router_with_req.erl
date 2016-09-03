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
