%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
