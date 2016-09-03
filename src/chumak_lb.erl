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

%% @doc ZeroMQ Simple Round-robin load-balancer

-module(chumak_lb).
-export([new/0, put/2, get/1, delete/2, is_empty/1]).

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
