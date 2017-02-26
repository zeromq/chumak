%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_command_test).

-include_lib("eunit/include/eunit.hrl").

decode_ping_test() ->
    Frame = <<4, "PING", 1, 0>>,
    {ok, Command, #{}} = chumak_command:decode(Frame, #{}),
    ?assertEqual(chumak_command:command_name(Command), ping).


decode_ready_test() ->
    BigProp = binary:copy(<<"luke">>, 100),
    Frame = <<
              5, "READY",                %% Command name "READY"
              11, "Socket-Type",         %% Property name "Socket-Type"
              0, 0, 0, 6, "DEALER",      %% Property value "DEALER"
              8, "Identity",             %% Property name "Identity"
              0, 0, 0, 5, "HELLO",       %% Property value "HELLO"
              8, "resource",             %% Property name "Resource"
              0, 0, 0, 11, "My-Resource",%% Property value "My-Resource"
              7, "X-Debug",              %% Property name for application use
              0, 0, 0, 8, "disabled",    %% Property value for application use
              5, "X-Big",                %% Property name for big property test
              0, 0, 1, 144, BigProp/binary %% Property value for big property test
            >>,
    {ok, Command, #{}} = chumak_command:decode(Frame, #{}),
    ?assertEqual(chumak_command:command_name(Command), ready),
    ?assertEqual(chumak_command:ready_socket_type(Command), "dealer"),
    ?assertEqual(chumak_command:ready_identity(Command), "HELLO"),
    ?assertEqual(chumak_command:ready_resource(Command), "My-Resource"),
    ?assertEqual(chumak_command:ready_metadata(Command), #{
                                                   "x-debug" => "disabled",
                                                   "x-big" => binary_to_list(BigProp)
                                                  }).

decode_wrong_ready_test() ->
    Frame = <<
              5, "READY",                %% Command name "READY"
              110, "Wrong or corrupted"  %% Wrong bytes
            >>,
    {error, wrong_ready_message} = chumak_command:decode(Frame, #{}).


encode_ready_command_test() ->
    Frame1 = chumak_command:encode_ready(req, "", "", #{}),
    ?assertMatch(<<
                   5, "READY",
                   11, "Socket-Type",
                   0, 0, 0, 3, "REQ", 
                   _/binary
                 >>, Frame1),

    Frame2 = chumak_command:encode_ready(rep, "my-name", "my-resource", #{}),
    ?assertEqual(Frame2, <<
                           5, "READY",
                           11, "Socket-Type",
                           0, 0, 0, 3, "REP",
                           8, "Identity",
                           0, 0, 0, 7, "my-name",
                           8, "Resource",
                           0, 0, 0, 11, "my-resource"
                         >>),
    Frame3 = chumak_command:encode_ready(rep, "my-name", "my-resource", #{"X-Host"=>"Host1"}),
    ?assertEqual(Frame3, <<
                           5, "READY",
                           11, "Socket-Type",
                           0, 0, 0, 3, "REP",
                           8, "Identity",
                           0, 0, 0, 7, "my-name",
                           8, "Resource",
                           0, 0, 0, 11, "my-resource",
                           6, "X-Host",
                           0, 0, 0, 5, "Host1"
                         >>),
    LargeProp = string:copies("large", 100),
    LargePropBin = list_to_binary(LargeProp),
    Frame4 = chumak_command:encode_ready(rep, "my-name", "my-resource", #{"X-Host"=>"Host1", "X-Large"=>LargeProp}),
    ?assertEqual(Frame4, <<
                           5, "READY",
                           11, "Socket-Type",
                           0, 0, 0, 3, "REP",
                           8, "Identity",
                           0, 0, 0, 7, "my-name",
                           8, "Resource",
                           0, 0, 0, 11, "my-resource",
                           6, "X-Host",
                           0, 0, 0, 5, "Host1",
                           7, "X-Large",
                           0, 0, 1, 244,
                           LargePropBin/binary>>).

decode_error_test() ->
    Frame = <<5, "ERROR", 19 ,"Invalid socket type">>,
    {ok, Command, #{}} = chumak_command:decode(Frame, #{}),
    ?assertEqual(chumak_command:command_name(Command), error),
    ?assertEqual(chumak_command:error_reason(Command), "Invalid socket type").

decode_invalid_error_test() ->
    Frame = <<5, "ERROR", 10, "Broken">>,
    {error, wrong_error_message} = chumak_command:decode(Frame, #{}).

encode_error_command_test() ->
    Frame = chumak_command:encode_error("Broken light-saber"),
    ?assertEqual(Frame, <<
                          5, "ERROR",
                          18, "Broken light-saber"
                        >>).

encode_subscribe_command_test() ->
    Frame = chumak_command:encode_subscribe(<<"debug">>),
    ?assertEqual(Frame, <<
                          9, "SUBSCRIBE",
                          "debug"
                        >>).


decode_subscribe_command_test() ->
    Frame = <<9, "SUBSCRIBE", "warn">>,
    {ok, Command, #{}} = chumak_command:decode(Frame, #{}),
    ?assertEqual(
       chumak_command:subscribe_subscription(Command),
       <<"warn">>
      ).

encode_cancel_command_test() ->
    Frame = chumak_command:encode_cancel(<<"error">>),
    ?assertEqual(Frame, <<
                          6, "CANCEL",
                          "error"
                        >>).


decode_subscribe_cancel_test() ->
    Frame = <<6, "CANCEL", "fatal">>,
    {ok, Command, #{}} = chumak_command:decode(Frame, #{}),
    ?assertEqual(
       chumak_command:cancel_subscription(Command),
       <<"fatal">>
      ).
