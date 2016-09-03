%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_lbs_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ?assertEqual(chumak_lbs:new(), {lbs, #{}, #{}}).

put_test() ->
    Q1 = chumak_lbs:new(),
    Q2 = chumak_lbs:put(Q1, "A", 1),
    ?assertEqual(Q2, {lbs,
                      #{"A" => [1]},
                      #{1 => "A"}}
                ),

    Q3 = chumak_lbs:put(Q2, "B", 3),
    ?assertEqual(Q3, {lbs,
                      #{"A" => [1], "B" => [3]},
                      #{1 => "A", 3 => "B"}}
                ).

get_test() ->
    Q1 = put_items(chumak_lbs:new(), [
                                         {a, 1},
                                         {b, 2},
                                         {a, 3},
                                         {b, 4},
                                         {c, 6}
                                        ]),
    {Q2, 3} = chumak_lbs:get(Q1, a),
    {Q3, 1} = chumak_lbs:get(Q2, a),
    {Q4, 3} = chumak_lbs:get(Q3, a),
    {Q5, 4} = chumak_lbs:get(Q4, b),
    {Q6, 2} = chumak_lbs:get(Q5, b),
    {Q7, 6} = chumak_lbs:get(Q6, c),
    ?assertEqual(Q7, {
                   lbs,
                   #{a => [1,3], b => [4,2], c => [6]},
                   #{1 => a, 2 => b, 3 => a, 4 => b, 6 => c}
                  }).

get_empty_test() ->
    ?assertEqual(chumak_lbs:get(chumak_lbs:new(), a), none).

delete_test() ->
    Q1 = put_items(chumak_lbs:new(), [
                                         {a, 1},
                                         {b, 2},
                                         {b, 3}
                                        ]),
    Q2 = chumak_lbs:delete(Q1, 1),
    Q3 = chumak_lbs:delete(Q1, 3),
    Q4 = chumak_lbs:delete(Q3, 2),
    Q5 = chumak_lbs:delete(Q4, 1),

    ?assertEqual({lbs, #{b => [3,2]}, #{2 => b, 3 => b}}, Q2),
    ?assertEqual({lbs, #{a => [1],b => [2]}, #{1 => a, 2 => b}}, Q3),
    ?assertEqual({lbs, #{a => [1]}, #{1 => a}}, Q4),
    ?assertEqual({lbs, #{}, #{}}, Q5).

put_items(LBs, []) ->
    LBs;
put_items(LBs, [{Key, Value} | Tail]) ->
    NewLBs = chumak_lbs:put(LBs, Key, Value),
    put_items(NewLBs, Tail).
