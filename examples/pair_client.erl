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
-module(pair_client).
-export([main/0]).

main() ->
    application:start(erlangzmq),
    {ok, Socket} = erlangzmq:socket(pair),

    case erlangzmq:connect(Socket, tcp, "localhost", 5555) of
        {ok, _BindPid} ->
            io:format("Connected OK with Pid: ~p\n", [Socket]);
        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p \n", [X])
    end,
    loop(Socket).

loop(Socket) ->
    {ok, Data1} = erlangzmq:recv_multipart(Socket),
    io:format("Received ~p\n", [Data1]),
    ok = erlangzmq:send_multipart(Socket, [<<"Hey Jude">>]),
    loop(Socket).
