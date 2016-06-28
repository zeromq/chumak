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
-module(resource_server).
-export([main/0]).

main() ->
    application:start(erlangzmq),
    {ok, Resource} = erlangzmq:resource(),
    {ok, SocketA} = erlangzmq:socket(rep, "A"),
    {ok, SocketB} = erlangzmq:socket(rep, "B"),

    erlangzmq:attach_resource(Resource, "service/a", SocketA),
    erlangzmq:attach_resource(Resource, "service/b", SocketB),

    spawn_link(fun () ->
                       loop(SocketA, <<"Hello A">>)
               end),
    spawn_link(fun () ->
                       loop(SocketB, <<"Hello B">>)
               end),
    {ok, _BindPid} = erlangzmq:bind(Resource, tcp, "localhost", 5555),

    receive
        _ -> ok
    end.

loop(Socket, Msg) ->
    Data = erlangzmq:recv(Socket),
    erlangzmq:send(Socket, <<"Reply from: ", Msg/binary, " is ", Msg/binary>>),
    loop(Socket, Msg).
