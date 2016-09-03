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

