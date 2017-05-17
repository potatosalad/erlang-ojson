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
-module('Elixir.OJSON.SyntaxError').

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	'__exception__' := true,
	message := nil | binary() | term(),
	token := nil | term(),
	pos := nil | non_neg_integer()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
-export([exception/1]).
-export([message/1]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		'__exception__' => true,
		message => nil,
		token => nil,
		pos => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

exception(List) when is_list(List) ->
	exception(maps:from_list(List));
exception(Attributes=#{ token := Token, pos := Pos }) when Token =/= nil ->
	Message = << "Unexpected token at position ", (to_string(Pos))/binary, ": ", (to_string(Token))/binary >>,
	'__struct__'(maps:put(message, Message, Attributes));
exception(Attributes=#{ pos := Pos }) ->
	Message = << "Unexpected end of input at position ", (to_string(Pos))/binary >>,
	'__struct__'(maps:put(message, Message, Attributes));
exception(Attributes) ->
	'__struct__'(Attributes).

message(#{ '__struct__' := ?MODULE, message := Message }) ->
	Message.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
to_string(V) when is_atom(V) ->
	erlang:atom_to_binary(V, unicode);
to_string(V) when is_binary(V) ->
	V;
to_string(V) when is_integer(V) ->
	erlang:integer_to_binary(V);
to_string(V) when is_list(V) ->
	erlang:iolist_to_binary(V).
