%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% @doc Very simple function to read a certificate.

-module(chumak_cert).

%% @doc Very simple function to read a certificate.
%%
%% Certificates are in ZPL format, identical to what is used by
%% pyzmq (the Python ZMQ implementation).
%%
%% Note that this implements the minimum to parse the Python certificates.

-export([read/1]).

-spec read(FileName::string()) -> [{public_key | private_key, binary()}] | {error, Reason::term()}.
read(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    find_curve_section(File).

find_curve_section(File) ->
    case file:read_line(File) of
        {ok, String} ->
            case re:run(String, "^curve *\\n") of
                {match, _} ->
                    find_keys(File, []);
                nomatch ->
                    find_curve_section(File)
            end;
        eof ->
            {error, no_curve_section}
    end.

find_keys(File, Acc) ->
    case file:read_line(File) of
        eof ->
            {ok, lists:reverse(Acc)};
        {ok, String} ->
            case parse_key(String) of
                {ok, Key} ->
                    find_keys(File, [Key | Acc]);
                continue ->
                    find_keys(File, Acc);
                end_of_section ->
                    {ok, lists:reverse(Acc)};
                Error ->
                    Error
            end
    end.

-define(VALUE_SPEC, "^ *= *\"(.*)\" *$").

parse_key("    public-key" ++ Value) ->
    case re:run(Value, ?VALUE_SPEC) of
        {match, [_, {Start, Length}]} ->
            KeyEncoded = string:substr(Value, Start + 1, Length),
            {ok, {public_key, chumak_z85:decode(KeyEncoded)}};
        _ ->
            {error, invalid_public_key_spec}
    end;
parse_key("    secret-key" ++ Value) ->
    case re:run(Value, ?VALUE_SPEC) of
        {match, [_, {Start, Length}]} ->
            KeyEncoded = string:substr(Value, Start + 1, Length),
            {ok, {secret_key, chumak_z85:decode(KeyEncoded)}};
        _ ->
            {error, invalid_secret_key_spec}
    end;
parse_key("    " ++ _) ->
    continue;
parse_key(Other) ->
    case re:run(Other, "^ *#") of
        {match, _} ->
            continue;
        _ ->
            end_of_section
    end.
