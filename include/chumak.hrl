%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Erlang common types for all modules

-export_type([transport/0, socket_type/0]).
-include_lib("kernel/include/logger.hrl").

-type transport() :: tcp.
-type socket_type() :: req | rep |
                     dealer | router |
                     pub | xpub |
                     sub | xsub |
                     push | pull |
                     pair.

-type z85_key() :: string().

-type socket_option()  :: curve_server     | %% true | false
                          curve_publickey  | %% binary()
                          curve_secretkey  | %% binary()
                          curve_serverkey  | %% binary()
                          curve_clientkeys.  %% [binary() | z85_key()]

-type security_mechanism() :: null |
                              curve.

-define(SOCKET_OPTS(Opts), lists:append([binary, {active, false}, {reuseaddr, true}], Opts)).
-define(GREETINGS_TIMEOUT, 1000).
-define(RECONNECT_TIMEOUT, 2000).
