%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_z85_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    Binary = <<16#86, 16#4f, 16#d2, 16#6f, 16#b5, 16#59, 16#f7, 16#5B>>,
    String = "HelloWorld",
    RandomNrs = [rand:uniform(256) - 1 || _ <- lists:seq(1, 100)],
    RandomBytes = << <<X>> || X <- RandomNrs >>,
    ?assertEqual(String, chumak_z85:encode(Binary)),
    ?assertEqual(Binary, chumak_z85:decode(String)),
    ?assertEqual(RandomBytes, 
                 chumak_z85:decode(chumak_z85:encode(RandomBytes))).
