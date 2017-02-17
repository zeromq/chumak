%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc This module interfaces to the encryption library.
%%
%% Two variants are supported: enacl and nacerl. Which variant 
%% is used is determined at the time of compilation. 
%%
%% If no encryption library is available, an error is thrown.

-module(chumak_curve_if).

-export([randombytes/1,
         box_keypair/0,
         box/4,
         box_open/4]).

-ifdef(CHUMAK_CURVE_LIB_NACERL).
-define(CURVE_MOD, nacerl).
-endif.

-ifdef(CHUMAK_CURVE_LIB_ENACL).
-define(CURVE_MOD, nacerl).
-endif.

-ifdef(CHUMAK_CURVE_LIB_NONE).
-define(CURVE_MOD, none).
-endif.

-ifndef(CURVE_MOD).
-define(CURVE_MOD, none).
-endif.

-spec randombytes(Size::integer()) -> binary().
randombytes(Size) ->
    case ?CURVE_MOD of
        none ->
            throw(not_supported);
        _ ->
            ?CURVE_MOD:randombytes(Size)
    end.

%% @doc Generate a key pair.
-spec box_keypair() -> #{secret => binary(), public => binary()}.
box_keypair() ->
    case ?CURVE_MOD of
        none ->
            throw(not_supported);
        _ ->
            ?CURVE_MOD:box_keypair()
    end.

%% @doc Encrypts+authenticates a message to another party.
%%
%% Encrypt a Message to the party identified by that party's public key using your own 
%% secret key to authenticate yourself. Requires a `Nonce' in addition. Returns the 
%% encrypted message.
%% @end
-spec box(Message::binary(),
          Nonce::binary(),
          SecretKey::binary(),
          PublicKey::binary()) -> binary().
box(Message, Nonce, SecretKey, PublicKey) ->
    case ?CURVE_MOD of
        none ->
            throw(not_supported);
        _ ->
            ?CURVE_MOD:box(Message, Nonce, SecretKey, PublicKey)
    end.

%% @doc Decrypts+verifies a message from another party.
%%
%% Decrypt a message (`Box') into a message given the other party's public key and your secret
%% key. Also requires the same nonce as was used by the other party. Returns the plaintext
%% message.
%% @end
-spec box_open(Box::binary(),
               Nonce::binary(),
               SecretKey::binary(),
               PublicKey::binary()) -> binary().
box_open(Box, Nonce, SecretKey, PublicKey) ->
    case ?CURVE_MOD of
        none ->
            throw(not_supported);
        _ ->
            ?CURVE_MOD:box_open(Box, Nonce, SecretKey, PublicKey)
    end.
