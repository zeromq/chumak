%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Subscription manager

-module(chumak_subscriptions).
-include_lib("kernel/include/logger.hrl").

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
                          {0, _} ->
                              true;
                          _ ->
                              false
                      end
              end, PeerSubscriptions).
