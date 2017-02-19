%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(chumak_protocol_curve_test).

-include_lib("eunit/include/eunit.hrl").

messages_test() ->
    #{public := ClientPublicKey, 
      secret := ClientSecretKey} = chumak_curve_if:box_keypair(),
    #{public := ServerPublicKey, 
      secret := ServerSecretKey} = chumak_curve_if:box_keypair(),
    #{public := ClientPublicTransientKey, 
      secret := ClientSecretTransientKey} = chumak_curve_if:box_keypair(),
    #{public := ServerPublicTransientKey, 
      secret := ServerSecretTransientKey} = chumak_curve_if:box_keypair(),
    #{public := CookiePublicKey, 
      secret := CookieSecretKey} = chumak_curve_if:box_keypair(),
    ClientShortNonce = 1,
    ServerShortNonce = 1,
    ServerSecData1 = #{mechanism => curve,
                       role => server,
                       server_public_transient_key =>
                          ServerPublicTransientKey,
                       server_secret_transient_key =>
                          ServerSecretTransientKey,
                       curve_secretkey => ServerSecretKey,
                       cookie_public_key => CookiePublicKey,
                       cookie_secret_key => CookieSecretKey,
                       server_nonce => ServerShortNonce},
    ServerDec1 = chumak_protocol:new_decoder(ServerSecData1),
    ClientSecData1 = #{mechanism => curve,
                       role => client,
                       client_public_transient_key =>
                          ClientPublicTransientKey,
                       client_secret_transient_key =>
                          ClientSecretTransientKey,
                       curve_publickey => ClientPublicKey,
                       curve_secretkey => ClientSecretKey,
                       curve_serverkey => ServerPublicKey,
                       client_nonce => ClientShortNonce},
    ClientDec1 = chumak_protocol:new_decoder(ClientSecData1),
    %% Client receives greeting
    ServerGreeting = chumak_protocol:build_greeting_frame(true, curve),
    {ready, ClientDec2} = chumak_protocol:decode(ClientDec1, ServerGreeting),
    %% Server receives greeting
    ClientGreeting = chumak_protocol:build_greeting_frame(false, curve),
    {ready, ServerDec2} = chumak_protocol:decode(ServerDec1, ClientGreeting),

    %% Client sends Hello
    {Hello, ClientSecData2} = chumak_protocol:build_hello_frame(ClientSecData1),
    ClientDec3 = chumak_protocol:set_decoder_security_data(ClientDec2, 
                                                           ClientSecData2),
    {ok, ServerDec3, [{hello}]} = chumak_protocol:decode(ServerDec2, Hello),
    ServerSecData2 = chumak_protocol:decoder_security_data(ServerDec3),
    #{client_nonce := ClientNonceDecoded,
      client_public_transient_key := CPTKDecoded} = ServerSecData2,
    ?assertEqual(ClientShortNonce, ClientNonceDecoded),
    ?assertEqual(ClientPublicTransientKey, CPTKDecoded),

    %% Server sends Welcome
    {Welcome, ServerSecData3} = chumak_protocol:build_welcome_frame(ServerSecData2),

    %% Server discards transient keys
    ?assertMatch(#{client_public_transient_key := <<>>,
                   server_public_transient_key := <<>>,
                   server_secret_transient_key := <<>>}, ServerSecData3),

    ServerDec4 = chumak_protocol:set_decoder_security_data(ServerDec3, 
                                                           ServerSecData3),

    %% Client decodes Welcome
    {ok, ClientDec4, [_WelcomeRec]} = chumak_protocol:decode(ClientDec3, 
                                                            Welcome),
    ClientSecData3 = chumak_protocol:decoder_security_data(ClientDec4),
    #{server_public_transient_key := SPTKDecoded} = ClientSecData3,
    ?assertEqual(ServerPublicTransientKey, SPTKDecoded),
    %% Client sends Initiate
    {Initiate, ClientSecData4} = 
        chumak_protocol:build_initiate_frame([], ClientSecData3),
    ClientDec5 = chumak_protocol:set_decoder_security_data(ClientDec4, 
                                                           ClientSecData4),
    %% Server decodes Initiate
    {ok, ServerDec5, [_InitiateRec]} = chumak_protocol:decode(ServerDec4, 
                                                             Initiate),
    ServerSecData4 = chumak_protocol:decoder_security_data(ServerDec5),
    #{client_public_permanent_key := CPPKDecoded,
      server_nonce := ServerNonce} = ServerSecData4,
    ?assertEqual(ClientPublicKey, CPPKDecoded),

    %% Server sends Ready
    {Ready, ServerSecData5} = 
        chumak_protocol:build_ready_frame(
            [], ServerSecData4#{server_nonce => ServerNonce + 1}),
    ServerDec6 = chumak_protocol:set_decoder_security_data(ServerDec5, 
                                                           ServerSecData5),
    %% Client decodes Ready
    {ok, ClientDec6, [ReadyRec]} = chumak_protocol:decode(ClientDec5, 
                                                   Ready),
    ?assertEqual(element(1, ReadyRec), ready),
    ClientSecData5 = chumak_protocol:decoder_security_data(ClientDec6),
    #{server_nonce := ServerNonceDecoded} = ClientSecData5,
    ?assertEqual(2, ServerNonceDecoded),

    %% Client sends message
    MessageText = <<"this is a test">>,
    {Message, _ClientSecData6} = 
        chumak_protocol:encode_message_multipart([MessageText], curve,
                                                 ClientSecData5),
    %% Server receives message
    {ok, _, [{message, DecodedMessage, _}]}  = chumak_protocol:decode(ServerDec6, Message),
    ?assertEqual(MessageText, DecodedMessage).
