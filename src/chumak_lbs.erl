%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc ZeroMQ Round-robin load-balancer based on an identifier

-module(chumak_lbs).
-export([new/0, put/3, get/2, delete/2, iterator/1, next/1]).

-record(lbs, {
          map  :: map(),  %% map of load balancers
          xref            %% reverse reference useful to locate identity at deletion
}).
-type lbs() :: #lbs{}.
-type lbs_iterator() :: maps:ierator().


%% @doc returns an empty load-balancer by identifier
-spec new() -> NewLBs::lbs().
new() ->
    #lbs{map=#{}, xref=#{}}.

%% @doc put a item to be balanced.
-spec put(LBs::lbs(), Identifier::term(), Item::term()) -> NewLB::lbs().
put(#lbs{map=LBMap, xref=XRef}=LBs, Identifier, Item) ->
    LB = get_sub_lb(LBMap, Identifier),
    NewLB = chumak_lb:put(LB, Item),

    NewLBMap = LBMap#{Identifier => NewLB},
    NewXRef = XRef#{Item => Identifier},

    LBs#lbs{map=NewLBMap, xref=NewXRef}.

%% @doc get the next available item from load-balancer, return none if empty.
-spec get(LBs::lbs(), Identifier::term()) -> none | {NewLBs::lbs(), Item::term()}.
get(#lbs{map=LBMap}=LBs, Identifier) ->
    LB = get_sub_lb(LBMap, Identifier),

    case chumak_lb:get(LB) of
        {NewLB, Item} ->
            NewLBMap = LBMap#{Identifier => NewLB},
            NewLBs = LBs#lbs{map=NewLBMap},
            {NewLBs, Item};

        none ->
            none
    end.

%% @doc remove item from load-balancer
-spec delete(LBs::lbs(), Item::term()) -> NewLBs::lbs().
delete(#lbs{xref=LBXRef}=LBs, Item)->
    case maps:find(Item, LBXRef) of
        {ok, Identifier} ->
            delete_by_identifier(LBs, Identifier, Item);
        error ->
            LBs
    end.

%% @doc create iterator over load-balancer identifiers
-spec iterator(LBs::lbs()) -> lbs_iterator().
iterator(#lbs{map=Map}) ->
  maps:iterator(Map).

%% @doc retrieve next key-value association in load-balancer identifiers
-spec next(Iter::lbs_iterator()) -> {Key::term(), Val::term(), NextIter::lbs_iterator()} | none.
next(Iter) ->
  maps:next(Iter).

%% Private API
delete_by_identifier(#lbs{map=LBMap, xref=LBXRef}=LBs, Identifier, Item) ->
    LB = get_sub_lb(LBMap, Identifier),
    NewLB = chumak_lb:delete(LB, Item),

    NewLBMap = case chumak_lb:is_empty(NewLB) of
                   true ->
                       maps:remove(Identifier, LBMap);
                   false ->
                       LBMap#{Identifier => NewLB}
               end,

    NewXRef = maps:remove(Item, LBXRef),
    LBs#lbs{map=NewLBMap, xref=NewXRef}.

get_sub_lb(LbsMap, Identity) ->
    case maps:find(Identity, LbsMap) of
        {ok, X} ->
            X;
        error ->
            chumak_lb:new()
    end.
