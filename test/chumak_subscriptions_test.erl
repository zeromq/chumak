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

-module(chumak_subscriptions_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ?assertEqual(chumak_subscriptions:new(), #{}).

put_test() ->
    S1 = chumak_subscriptions:new(),
    S2 = chumak_subscriptions:put(S1, self(), <<"A">>),
    ?assertEqual(#{self() => [<<"A">>]}, S2),

    S3 = chumak_subscriptions:put(S2, self(), <<"B">>),
    ?assertEqual(#{self() => [<<"A">>, <<"B">>]}, S3).


delete_by_peer_and_subscription_test() ->
    S1 = chumak_subscriptions:new(),
    S2 = chumak_subscriptions:put(S1, self(), <<"A">>),
    S3 = chumak_subscriptions:put(S2, self(), <<"B">>),
    S4 = chumak_subscriptions:delete(S3, self(), <<"A">>),
    S5 = chumak_subscriptions:delete(S3, self(), <<"C">>),
    ?assertEqual(#{self() => [<<"B">>]}, S4),
    ?assertEqual(#{self() => [<<"A">>, <<"B">>]}, S5).


delete_by_peer_test() ->
    S1 = chumak_subscriptions:new(),
    S2 = chumak_subscriptions:put(S1, self(), <<"A">>),
    S3 = chumak_subscriptions:put(S2, self(), <<"B">>),
    S4 = chumak_subscriptions:delete(S3, self()),
    ?assertEqual(#{}, S4).

match_test() ->
    OtherPid = spawn_link(fun () -> ok end),
    S1 = chumak_subscriptions:new(),
    S2 = chumak_subscriptions:put(S1, self(), <<"A">>),
    S3 = chumak_subscriptions:put(S2, self(), <<"B">>),
    S4 = chumak_subscriptions:put(S3, OtherPid, <<"D">>),
    S5 = chumak_subscriptions:put(S4, self(), <<"AB">>),
    S6 = chumak_subscriptions:put(S5, OtherPid, <<>>),

    M1 = chumak_subscriptions:match(S5, <<"A">>),
    M2 = chumak_subscriptions:match(S5, <<"B">>),
    M3 = chumak_subscriptions:match(S5, <<"C">>),
    M4 = chumak_subscriptions:match(S5, <<"D">>),
    M5 = chumak_subscriptions:match(S5, <<"AB">>),
    M6 = chumak_subscriptions:match(S6, <<"W">>),

    ?assertEqual([self()], M1),
    ?assertEqual([self()], M2),
    ?assertEqual([], M3),
    ?assertEqual([OtherPid], M4),
    ?assertEqual([self()], M5),
    ?assertEqual([OtherPid], M6).
