%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_acceptance_version).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 3010).

single_test_() ->
    [
     {
       "Should return valid version when started",
       {setup, fun version_error/0, fun start/1, fun version_ok/1}
     }
    ].

version_error() ->
  ?_assertEqual({error, application_not_started}, chumak:version()).

start(_) ->
    application:ensure_started(chumak).


version_ok(_) ->
  ?_assertMatch({ok, _}, chumak:version()).

