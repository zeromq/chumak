%% @copyright 2016 Choven Corp.
%%
%% This file is part of erlangzmq.
%%
%% erlangzmq is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% erlangzmq is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with erlangzmq.  If not, see <http://www.gnu.org/licenses/>

-module(erlangzmq_command_test).

-include_lib("eunit/include/eunit.hrl").

decode_ping_test() ->
    Frame = <<4, "PING", 1, 0>>,
    {ok, Command} = erlangzmq_command:decode(Frame),
    ?assertEqual(erlangzmq_command:command_name(Command), ping).


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
    {ok, Command} = erlangzmq_command:decode(Frame),
    ?assertEqual(erlangzmq_command:command_name(Command), ready),
    ?assertEqual(erlangzmq_command:ready_socket_type(Command), dealer),
    ?assertEqual(erlangzmq_command:ready_identity(Command), "HELLO"),
    ?assertEqual(erlangzmq_command:ready_resource(Command), "My-Resource"),
    ?assertEqual(erlangzmq_command:ready_metadata(Command), #{
                                                   "x-debug" => "disabled",
                                                   "x-big" => binary_to_list(BigProp)
                                                  }).

decode_wrong_ready_test() ->
    Frame = <<
              5, "READY",                %% Command name "READY"
              110, "Wrong or corrupted"  %% Wrong bytes
            >>,
    {error, wrong_ready_message} = erlangzmq_command:decode(Frame).


encode_ready_command_test() ->
    Frame1 = erlangzmq_command:encode_ready(req, "", "", #{}),
    ?assertEqual(Frame1, <<
                           5, "READY",
                           11, "Socket-Type",
                           0, 0, 0, 3, "REQ"
                         >>),

    Frame2 = erlangzmq_command:encode_ready(rep, "my-name", "my-resource", #{}),
    ?assertEqual(Frame2, <<
                           5, "READY",
                           11, "Socket-Type",
                           0, 0, 0, 3, "REP",
                           8, "Identity",
                           0, 0, 0, 7, "my-name",
                           8, "Resource",
                           0, 0, 0, 11, "my-resource"
                         >>),
    Frame3 = erlangzmq_command:encode_ready(rep, "my-name", "my-resource", #{"X-Host"=>"Host1"}),
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
    Frame4 = erlangzmq_command:encode_ready(rep, "my-name", "my-resource", #{"X-Host"=>"Host1", "X-Large"=>LargeProp}),
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
    {ok, Command} = erlangzmq_command:decode(Frame),
    ?assertEqual(erlangzmq_command:command_name(Command), error),
    ?assertEqual(erlangzmq_command:error_reason(Command), "Invalid socket type").

decode_invalid_error_test() ->
    Frame = <<5, "ERROR", 10, "Broken">>,
    {error, wrong_error_message} = erlangzmq_command:decode(Frame).

encode_error_command_test() ->
    Frame = erlangzmq_command:encode_error("Broken light-saber"),
    ?assertEqual(Frame, <<
                          5, "ERROR",
                          18, "Broken light-saber"
                        >>).

encode_subscribe_command_test() ->
    Frame = erlangzmq_command:encode_subscribe(<<"debug">>),
    ?assertEqual(Frame, <<
                          9, "SUBSCRIBE",
                          "debug"
                        >>).


decode_subscribe_command_test() ->
    Frame = <<9, "SUBSCRIBE", "warn">>,
    {ok, Command} = erlangzmq_command:decode(Frame),
    ?assertEqual(
       erlangzmq_command:subscribe_subscription(Command),
       <<"warn">>
      ).

encode_cancel_command_test() ->
    Frame = erlangzmq_command:encode_cancel(<<"error">>),
    ?assertEqual(Frame, <<
                          6, "CANCEL",
                          "error"
                        >>).


decode_subscribe_cancel_test() ->
    Frame = <<6, "CANCEL", "fatal">>,
    {ok, Command} = erlangzmq_command:decode(Frame),
    ?assertEqual(
       erlangzmq_command:cancel_subscription(Command),
       <<"fatal">>
      ).
