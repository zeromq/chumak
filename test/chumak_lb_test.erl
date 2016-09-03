%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_lb_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ?assertEqual(chumak_lb:new(), []).

put_test() ->
    Q1 = chumak_lb:new(),
    Q2 = chumak_lb:put(Q1, 1),
    ?assertEqual(Q2, [1]),
    Q3 = chumak_lb:put(Q2, 3),
    ?assertEqual(Q3, [3, 1]).

get_test() ->
    Q1 = chumak_lb:put(
          chumak_lb:put(chumak_lb:new(), 1),
          3),
    {Q2, 3} = chumak_lb:get(Q1),
    {Q3, 1} = chumak_lb:get(Q2),
    {Q4, 3} = chumak_lb:get(Q3),
    ?assertEqual(Q4, [1, 3]).

get_empty_test() ->
    ?assertEqual(chumak_lb:get(chumak_lb:new()), none).

delete_test() ->
    Q1 = chumak_lb:put(
          chumak_lb:put(chumak_lb:new(), 1),
          3),
    Q2 = chumak_lb:delete(Q1, 1),
    Q3 = chumak_lb:delete(Q1, 3),

    ?assertEqual(Q2, [3]),
    ?assertEqual(Q3, [1]).
