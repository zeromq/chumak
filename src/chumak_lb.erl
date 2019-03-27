%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Simple Round-robin load-balancer

-module(chumak_lb).
-export([new/0, put/2, get/1, delete/2, is_empty/1, to_list/1]).

-type lb() :: list().

%% @doc returns an empty load-balancer
-spec new() -> NewLB::lb().
new() ->
    [].

%% @doc put a item to be balanced.
-spec put(LB::lb(), Item::term()) -> NewLB::lb().
put(LB, Item) ->
    [Item|LB].

%% @doc get the next available item from load-balancer, return none if empty.
-spec get(LB::lb()) -> none | {NewLB::lb(), Item::term()}.
get([]) ->
    none;
get([Head|Tail]) ->
    {Tail ++ [Head], Head}.

%% @doc remove item from load-balancer
-spec delete(LB::lb(), Item::term()) -> NewLB::lb().
delete(LB, Item)->
    lists:delete(Item, LB).

%% @doc return if true or false this LB is empty
-spec is_empty(LB::lb()) -> true | false.
is_empty([]) -> true;
is_empty(_)  -> false.

to_list(LB) ->
  LB.
