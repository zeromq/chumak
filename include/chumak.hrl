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

%% @doc Erlang common types for all modules

-export_type([transport/0, socket_type/0]).

-type transport() :: tcp.
-type socket_type() :: req | rep |
                     dealer | router |
                     pub | xpub |
                     sub | xsub |
                     push | pull |
                     pair.
-define(SOCKET_OPTS(Opts), lists:append([binary, {active, false}, {reuseaddr, true}], Opts)).
-define(GREETINGS_TIMEOUT, 1000).
-define(RECONNECT_TIMEOUT, 2000).
