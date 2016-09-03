%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_socket_test).

-include_lib("eunit/include/eunit.hrl").

init_with_invalid_pattern_test() ->
    {stop, Reason} = chumak_socket:init({foo, "my-identity"}),
    ?assertEqual(Reason, invalid_socket_type).
