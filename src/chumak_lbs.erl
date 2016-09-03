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

%% @doc ZeroMQ Round-robin load-balancer based on an identifier

-module(chumak_lbs).
-export([new/0, put/3, get/2, delete/2]).

-record(lbs, {
          map  :: map(),  %% map of load balancers
          xref            %% reverse reference useful to locate identity at deletion
}).
-type lbs() :: #lbs{}.


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
