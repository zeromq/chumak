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

%% @doc ZeroMQ Subscription manager

-module(erlangzmq_subscriptions).
-export([new/0, put/3, delete/2, delete/3, match/2]).


-type subscriptions() :: #{PeerPid::pid => [Subscription::binary()]}.

%% @doc returns new subscriptions
-spec new() -> NewSubscriptions::subscriptions().
new() ->
    #{}.

%% @doc put a new subscription
-spec put(Subscriptions1::subscriptions(), PeerPid::pid(), Subscription::binary()) -> Subscriptions2::subscriptions().
put(Subscriptions, PeerPid, Subscription) ->
    PeerSubscriptions = maps:get(PeerPid, Subscriptions, []) ++ [Subscription],
    maps:put(PeerPid, PeerSubscriptions, Subscriptions).

%% @doc delete a new subscription by peer-pid and subscription
-spec delete(Subscriptions1::subscriptions(), PeerPid::pid(), Subscription::binary()) -> Subscriptions2::subscriptions().
delete(Subscriptions, PeerPid, Subscription) ->
    PeerSubscriptions1 = maps:get(PeerPid, Subscriptions, []),
    PeerSubscriptions2 = lists:delete(Subscription, PeerSubscriptions1),

    case PeerSubscriptions2 of
        [] ->
            maps:remove(PeerPid, Subscriptions);
        _ ->
            maps:put(PeerPid, PeerSubscriptions2, Subscriptions)
    end.

%% @doc delete a new subscription by peer-pid
-spec delete(Subscriptions1::subscriptions(), PeerPid::pid()) -> Subscriptions2::subscriptions().
delete(Subscriptions, PeerPid) ->
    maps:remove(PeerPid, Subscriptions).

%% @doc return the list of peers that matches with subscription
-spec match(Subscriptions::subscriptions(), FirstPart::binary()) -> [PeerPid::pid()].
match(Subscriptions, FirstPart) ->
    PeerPids = maps:keys(Subscriptions),
    lists:filter(fun (PeerPid) ->
                         peer_match(Subscriptions, PeerPid, FirstPart)
                 end, PeerPids).

%% Private API
peer_match(Subscriptions, PeerPid, FirstPart) ->
    PeerSubscriptions = maps:get(PeerPid, Subscriptions, []),
    lists:any(fun (<<>>) ->
                      true;

                  (PeerSubscription) ->
                      case binary:match(FirstPart, PeerSubscription) of
                          nomatch ->
                              false;
                          _ ->
                              true
                      end
              end, PeerSubscriptions).
