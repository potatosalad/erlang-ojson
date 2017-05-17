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
-module('Elixir.OJSON.EncodeError').

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	'__exception__' := true,
	message := nil | binary() | term(),
	value := nil | term()
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
		value => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

exception(Message) when is_binary(Message) ->
	exception(#{ message => Message });
exception(Attributes) ->
	'__struct__'(Attributes).

message(#{ '__struct__' := ?MODULE, message := nil, value := Value }) ->
	<< "unable to encode value: ", (inspect(Value))/binary >>;
message(#{ '__struct__' := ?MODULE, message := Message }) ->
	Message.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
inspect(Term) ->
	_ = code:ensure_loaded('Elixir.Kernel'),
	case erlang:function_exported('Elixir.Kernel', inspect, 1) of
		true ->
			'Elixir.Kernel':inspect(Term);
		false ->
			erlang:iolist_to_binary(io_lib:format("~p", [Term]))
	end.
