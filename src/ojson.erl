%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  17 May 2017 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(ojson).

%% Types
-type decoder_options() :: #{
	keys => atoms | 'atoms!'
} | [{atom(), term()}].

-export_type([decoder_options/0]).

-type encoder_options() :: #{
	escape => html_safe | javascript | unicode,
	iodata => boolean(),
	strict_keys => boolean()
} | [{atom(), term()}].

-export_type([encoder_options/0]).

%% API
-export([decode/1]).
-export([decode/2]).
-export(['decode!'/1]).
-export(['decode!'/2]).
-export([encode/1]).
-export([encode/2]).
-export(['encode!'/1]).
-export(['encode!'/2]).
-export([encode_to_iodata/1]).
-export([encode_to_iodata/2]).
-export(['encode_to_iodata!'/1]).
-export(['encode_to_iodata!'/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec decode(iodata())
	-> {ok, ojson_decoder:t()} | {error, {invalid, non_neg_integer()}} | {error, {invalid, binary(), non_neg_integer}}.
decode(Input) ->
	decode(Input, #{}).

-spec decode(iodata(), decoder_options())
	-> {ok, ojson_decoder:t()} | {error, {invalid, non_neg_integer()}} | {error, {invalid, binary(), non_neg_integer}}.
decode(Input, Options) when is_map(Options) ->
	ojson_decoder:parse(Input, Options);
decode(Input, Options) when is_list(Options) ->
	decode(Input, maps:from_list(Options)).

-spec 'decode!'(iodata())
	-> ojson_decoder:t() | no_return().
'decode!'(Input) ->
	'decode!'(Input, #{}).

-spec 'decode!'(iodata(), decoder_options())
	-> ojson_decoder:t() | no_return().
'decode!'(Input, Options) when is_map(Options) ->
	ojson_decoder:'parse!'(Input, Options);
'decode!'(Input, Options) when is_list(Options) ->
	'decode!'(Input, maps:from_list(Options)).

-spec encode(Value)
	-> {ok, iodata()} | {ok, binary()} | {error, {invalid, any()}}
	when Value :: term().
encode(Value) ->
	encode(Value, #{}).

-spec encode(Value, Options)
	-> {ok, iodata()} | {ok, binary()} | {error, {invalid, any()}}
	when Value :: term(), Options :: encoder_options().
encode(Value, Options=#{ iodata := true }) ->
	encode_to_iodata(Value, Options);
encode(Value, Options) when is_map(Options) ->
	try 'encode!'(Value, Options) of
		JSON ->
			{ok, JSON}
	catch
		error:Exception=#{ '__struct__' := 'Elixir.OJSON.EncodeError', value := V } ->
			_ = Exception,
			{error, {invalid, V}}
	end;
encode(Value, Options) when is_list(Options) ->
	encode(Value, maps:from_list(Options)).

-spec 'encode!'(Value)
	-> iodata() | binary() | no_return()
	when Value :: term().
'encode!'(Value) ->
	'encode!'(Value, #{}).

-spec 'encode!'(Value, Options)
	-> iodata() | binary() | no_return()
	when Value :: term(), Options :: encoder_options().
'encode!'(Value, Options=#{ iodata := true }) ->
	'encode_to_iodata!'(Value, Options);
'encode!'(Value, Options) when is_map(Options) ->
	erlang:iolist_to_binary('encode_to_iodata!'(Value, Options));
'encode!'(Value, Options) when is_list(Options) ->
	'encode!'(Value, maps:from_list(Options)).

-spec encode_to_iodata(Value)
	-> {ok, iodata()} | {error, {invalid, any()}}
	when Value :: term().
encode_to_iodata(Value) ->
	encode_to_iodata(Value, #{}).

-spec encode_to_iodata(Value, Options)
	-> {ok, iodata()} | {error, {invalid, any()}}
	when Value :: term(), Options :: encoder_options().
encode_to_iodata(Value, Options) when is_map(Options) ->
	try 'encode_to_iodata!'(Value, Options) of
		JSON ->
			{ok, JSON}
	catch
		error:Exception=#{ '__struct__' := 'Elixir.OJSON.EncodeError', value := V } ->
			_ = Exception,
			{error, {invalid, V}}
	end;
encode_to_iodata(Value, Options) when is_list(Options) ->
	encode_to_iodata(Value, maps:from_list(Options)).

-spec 'encode_to_iodata!'(Value)
	-> iodata() | no_return()
	when Value :: term().
'encode_to_iodata!'(Value) ->
	'encode_to_iodata!'(Value, #{}).

-spec 'encode_to_iodata!'(Value, Options)
	-> iodata() | no_return()
	when Value :: term(), Options :: encoder_options().
'encode_to_iodata!'(Value, Options) when is_map(Options) ->
	ojson_encoder:encode(Value, Options);
'encode_to_iodata!'(Value, Options) when is_list(Options) ->
	'encode_to_iodata!'(Value, maps:from_list(Options)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
