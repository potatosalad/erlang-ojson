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
-module(ojson_encoder).

%% API
-export([encode/2]).
-export([encode_atom/2]).
-export([encode_bitstring/2]).
-export([encode_float/2]).
-export([encode_integer/2]).
-export([encode_key/1]).
-export([encode_list/2]).
-export([encode_map/2]).
-export([encode_struct/2]).

%% Macros
-define(raise(V),
	erlang:error('Elixir.OJSON.EncodeError':exception(#{ value => V }))).
-define(raise(V, M),
	erlang:error('Elixir.OJSON.EncodeError':exception(#{ value => V, message => M }))).
-define(seq(C),
	case erlang:integer_to_binary(C, 16) of
		<< C0 >> -> << $\\, $u, $0, $0, $0, C0 >>;
		<< C0, C1 >> -> << $\\, $u, $0, $0, C0, C1 >>;
		<< C0, C1, C2 >> -> << $\\, $u, $0, C0, C1, C2 >>;
		<< C0, C1, C2, C3 >> -> << $\\, $u, C0, C1, C2, C3 >>
	end).

%%%===================================================================
%%% API functions
%%%===================================================================

encode(Value, Options) when is_atom(Value) ->
	encode_atom(Value, Options);
encode(Value, Options) when is_bitstring(Value) ->
	encode_bitstring(Value, Options);
encode(Value, Options) when is_float(Value) ->
	encode_float(Value, Options);
encode(Value, Options) when is_integer(Value) ->
	encode_integer(Value, Options);
encode(Value, Options) when is_list(Value) ->
	encode_list(Value, Options);
encode(Value=#{ '__struct__' := _ }, Options) ->
	encode_struct(Value, Options);
encode(Value, Options) when is_map(Value) ->
	encode_map(Value, Options);
encode(Value, Options) ->
	_ = code:ensure_loaded('Elixir.OJSON.Encoder'),
	case erlang:function_exported('Elixir.OJSON.Encoder', encode, 2) of
		true ->
			'Elixir.OJSON.Encoder':encode(Value, Options);
		false ->
			?raise(Value)
	end.

encode_atom(nil, _) ->
	<<"null">>;
encode_atom(true, _) ->
	<<"true">>;
encode_atom(false, _) ->
	<<"false">>;
encode_atom(Atom, Options) ->
	encode_bitstring(erlang:atom_to_binary(Atom, unicode), Options).

encode_bitstring(<<>>, _) ->
	<< $", $" >>;
encode_bitstring(String, Options) ->
	[$", escape(String, maps:get(escape, Options, nil)), $"].

encode_float(Float, _) ->
	io_lib_format:fwrite_g(Float).

encode_integer(Integer, _) ->
	erlang:integer_to_binary(Integer).

encode_key(Value) ->
	_ = code:ensure_loaded('Elixir.String.Chars'),
	case erlang:function_exported('Elixir.String.Chars', impl_for, 1) of
		true ->
			case 'Elixir.String.Chars':impl_for(Value) of
				nil ->
					?raise(Value, << "excpeted a String.Chars encodable value, got: ", (inspect(Value))/binary >>);
				Impl ->
					Impl:to_string(Value)
			end;
		false ->
			case Value of
				_ when is_atom(Value) ->
					erlang:atom_to_binary(Value, unicode);
				_ when is_bitstring(Value) ->
					Value;
				_ when is_float(Value) ->
					erlang:iolist_to_binary(io_lib_format:fwrite_g(Value));
				_ when is_integer(Value) ->
					erlang:integer_to_binary(Value);
				_ when is_list(Value) ->
					erlang:iolist_to_binary(Value);
				_ ->
					?raise(Value, << "excpeted a String.Chars encodable value, got: ", (inspect(Value))/binary >>)
			end
	end.

encode_list([], _) ->
	<< $[, $] >>;
encode_list(List, Options) ->
	encode_list(List, maps:get(pretty, Options, false), Options).

encode_map(Map, _) when map_size(Map) == 0 ->
	<< ${, $} >>;
encode_map(Map0, Options) ->
	Map = encode_map_keys(Map0, Options),
	encode_map(Map, maps:get(pretty, Options, false), Options).

encode_struct(Map=#{ '__struct__' := _ }, Options) ->
	_ = code:ensure_loaded('Elixir.OJSON.Encoder'),
	case erlang:function_exported('Elixir.OJSON.Encoder', encode, 2) of
		true ->
			'Elixir.OJSON.Encoder':encode(Map, Options);
		false ->
			encode_map(maps:remove('__struct__', Map), Options)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
chunk_size(<< C, _/binary >>, _Mode, Acc) when C =< 16#1F orelse C == $" orelse C == $\\ ->
	Acc;
chunk_size(<< C, Rest/binary >>, Mode, Acc) when C < 16#80 ->
	chunk_size(Rest, Mode, Acc + 1);
chunk_size(<< _/utf8, _/binary >>, unicode, Acc) ->
	Acc;
chunk_size(<< C/utf8, _/binary >>, Mode, Acc) when (Mode == html_safe orelse Mode == javascript) andalso (C == 16#2028 orelse C == 16#2029) ->
	Acc;
chunk_size(<< C/utf8, Rest/binary >>, Mode, Acc) ->
	Size =
		if
			C < 16#800 -> 2;
			C < 16#10000 -> 3;
			true -> 4
		end,
	chunk_size(Rest, Mode, Acc + Size);
chunk_size(<< C >>, _, _) ->
	?raise(<< C >>);
chunk_size(<<>>, _, Acc) ->
	Acc.

%% @private
encode_list(List, true, Options0) ->
	Indent = maps:get(indent, Options0, 2),
	Offset = maps:get(offset, Options0, 0) + Indent,
	Options = maps:put(offset, Offset, Options0),
	[[_ | Head] | Tail] =
		[begin
			[<< $,, $\n >>, binary:copy(<< $\s >>, Offset), encode(Element, Options)]
		end || Element <- List],
	[<< $[, $\n >>, Head, Tail, $\n, binary:copy(<< $\s >>, Offset - Indent), $]];
encode_list(List, _, Options) ->
	[[_ | Head] | Tail] =
		[begin
			[$,, encode(Element, Options)]
		end || Element <- List],
	[$[, Head, Tail, $]].

%% @private
encode_map(Map, true, Options0) ->
	Indent = maps:get(indent, Options0, 2),
	Offset = maps:get(offset, Options0, 0) + Indent,
	Options = maps:put(offset, Offset, Options0),
	Keys = lists:sort(maps:keys(Map)),
	[[_ | Head] | Tail] =
		[begin
			[<< $,, $\n >>, binary:copy(<< $\s >>, Offset), encode_bitstring(Key, Options), $:, $\s, encode(maps:get(Key, Map), Options)]
		end || Key <- Keys],
	[<< ${, $\n >>, Head, Tail, $\n, binary:copy(<< $\s >>, Offset - Indent), $}];
encode_map(Map, _, Options) ->
	Keys = lists:sort(maps:keys(Map)),
	[[_ | Head] | Tail] =
		[begin
			[$,, encode_bitstring(Key, Options), $:, encode(maps:get(Key, Map), Options)]
		end || Key <- Keys],
	[${, Head, Tail, $}].

%% @private
encode_map_keys(Map, #{ strict_keys := true }) ->
	maps:fold(fun fold_map_keys_strict/3, maps:new(), Map);
encode_map_keys(Map, _) ->
	Keys = lists:sort(maps:keys(Map)),
	lists:foldl(fun(Key, Acc) ->
		fold_map_keys(Key, maps:get(Key, Map), Acc)
	end, maps:new(), Keys).

%% @private
escape(<<>>, _) ->
	[];
escape(<< $", Rest/binary >>, Mode) ->
	[<< $\\, $" >> | escape(Rest, Mode)];
escape(<< $\\, Rest/binary >>, Mode) ->
	[<< $\\, $\\ >> | escape(Rest, Mode)];
escape(<< $\n, Rest/binary >>, Mode) ->
	[<< $\\, $n >> | escape(Rest, Mode)];
escape(<< $\t, Rest/binary >>, Mode) ->
	[<< $\\, $t >> | escape(Rest, Mode)];
escape(<< $\r, Rest/binary >>, Mode) ->
	[<< $\\, $r >> | escape(Rest, Mode)];
escape(<< $\f, Rest/binary >>, Mode) ->
	[<< $\\, $f >> | escape(Rest, Mode)];
escape(<< $\b, Rest/binary >>, Mode) ->
	[<< $\\, $b >> | escape(Rest, Mode)];
escape(<< C, Rest/binary >>, Mode) when C =< 16#1F orelse C == 16#7F ->
	[?seq(C) | escape(Rest, Mode)];
escape(<< C/utf8, Rest/binary >>, Mode) when C >= 16#80 andalso C =< 16#9F ->
	[?seq(C) | escape(Rest, Mode)];
escape(<< C/utf8, Rest/binary >>, Mode=unicode) when C >= 16#A0 andalso C =< 16#FFFF ->
	[?seq(C) | escape(Rest, Mode)];
escape(<< C/utf8, Rest/binary >>, Mode=unicode) when C >= 16#FFFF ->
	Code = C - 16#10000,
	[
		?seq(16#D800 bor (Code bsr 10)),
		?seq(16#DC00 bor (Code band 16#3FF))
		| escape(Rest, Mode)
	];
escape(<< C/utf8, Rest/binary >>, Mode) when (Mode == html_safe orelse Mode == javascript) andalso (C == 16#2028 orelse C == 16#2029) ->
	[?seq(C) | escape(Rest, Mode)];
escape(<< $/, Rest/binary >>, Mode=html_safe) ->
	[<< $\\, $/ >> | escape(Rest, Mode)];
escape(<< C/utf8, Rest/binary >>, Mode=html_safe) ->
	[<< C/utf8 >> | escape(Rest, Mode)];
escape(String, Mode) ->
	Size = chunk_size(String, Mode, 0),
	<< Chunk:Size/binary, Rest/binary >> = String,
	[Chunk | escape(Rest, Mode)].

%% @private
fold_map_keys(K, Value, Acc) ->
	Key = encode_key(K),
	maps:put(Key, Value, Acc).

%% @private
fold_map_keys_strict(K, Value, Acc) ->
	Key = encode_key(K),
	case maps:is_key(Key, Acc) of
		true ->
			?raise(Key, << "duplicate key found: ", (inspect(Key))/binary >>);
		false ->
			maps:put(Key, Value, Acc)
	end.

%% @private
inspect(Term) ->
	_ = code:ensure_loaded('Elixir.Kernel'),
	case erlang:function_exported('Elixir.Kernel', inspect, 1) of
		true ->
			'Elixir.Kernel':inspect(Term);
		false ->
			erlang:iolist_to_binary(io_lib:format("~p", [Term]))
	end.
