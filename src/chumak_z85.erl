%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Implements Z85 encoding and decoding, as specified in 
%% https://rfc.zeromq.org/spec:32/Z85/

-module(chumak_z85).

-define(SPECIAL_CHARS, ".-:+=^!/*?&<>()[]{}@%$#").

-export([encode/1, decode/1]).

-spec encode(binary()) -> string().
%% @doc Encode a binary to a Z85 string.
%%
%% As per the spec, the length of the binary SHALL be divisible by 4 
%% with no remainder.
encode(Binary) when is_binary(Binary) ->
    case size(Binary) rem 4 of
        0 -> 
            lists:flatten([encode_octets(X) || 
                           <<X:32/big-unsigned-integer>> <= Binary])
    end.

-spec decode(string()) -> binary().
%% @doc Decode a Z85 string to a binary.
%%
%% As per the spec, the length of the string SHALL be divisible by 5 
%% with no remainder.
%%
%% Note that not all strings are valid encodings. The result for a string that 
%% is not a valid encoding is unspecified.
decode(String) when is_list(String) ->
    case length(String) rem 5 of
        0 ->
            << <<X:32/big-unsigned-integer>> || 
              X <- to_nrs(String, [])>>
    end.

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------
to_nrs([], Acc) ->
    lists:reverse(Acc);
to_nrs([C1, C2, C3, C4, C5 | T], Acc) ->
    Number = decode_char(C5) + 
             decode_char(C4) * 85 + 
             decode_char(C3) * 85 * 85 +
             decode_char(C2) * 85 * 85 * 85 +
             decode_char(C1) * 85 * 85 * 85 * 85,
    to_nrs(T, [Number | Acc]).

decode_char(C) when $0 =< C, C =< $9 ->
    C - $0;
decode_char(C) when $a =< C, C =< $z ->
    C - $a + 10;
decode_char(C) when $A =< C, C =< $Z ->
    C - $A + 36;
decode_char(C) ->
    find_pos(C, ?SPECIAL_CHARS, 62).

find_pos(C, [C |_], N) ->
    N;
find_pos(C, [_ |T], N) ->
    find_pos(C, T, N+1).

encode_octets(Int) ->
    C1   = Int  rem 85,
    Int2 = Int  div 85,
    C2   = Int2 rem 85,
    Int3 = Int2 div 85,
    C3   = Int3 rem 85,
    Int4 = Int3 div 85,
    C4   = Int4 rem 85,
    C5   = Int4 div 85,
    [encode_85(C5), encode_85(C4), encode_85(C3), encode_85(C2), encode_85(C1)].

encode_85(X) when X =< 9 ->
    X + 48; % (0 => $0 = 48)
encode_85(X) when X =< 35 ->
    X + 87; % (10 => $a = 97)
encode_85(X) when X =< 61 ->
    X + 29; % (36 => $A = 36)
encode_85(X) when X >= 61 ->
    lists:nth(X - 61, ?SPECIAL_CHARS).
