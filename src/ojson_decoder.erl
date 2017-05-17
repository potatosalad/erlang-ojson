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
-module(ojson_decoder).

%% Types
-type t() :: nil | true | false | list() | float() | integer() | binary() | map().

-export_type([t/0]).

%% API
-export([parse/1]).
-export([parse/2]).
-export(['parse!'/1]).
-export(['parse!'/2]).

%% Macros
-define(raise(Pos),
	begin
		ST = erlang:get_stacktrace(),
		{current_stacktrace, [CST | _]} = erlang:process_info(erlang:self(), current_stacktrace),
		erlang:raise(error, 'Elixir.OJSON.SyntaxError':exception(#{ pos => Pos }), [CST | ST])
	end).
-define(raise(Pos, Token),
	begin
		ST = erlang:get_stacktrace(),
		{current_stacktrace, [CST | _]} = erlang:process_info(erlang:self(), current_stacktrace),
		erlang:raise(error, 'Elixir.OJSON.SyntaxError':exception(#{ pos => Pos, token => Token }), [CST | ST])
	end).
-define(syntax_error(Other, Pos, Token),
	case Other of
		<< Token/utf8, _/binary >> ->
			erlang:throw({invalid, << Token >>, Pos});
		_ ->
			erlang:throw({invalid, Pos})
	end).
-define(syntax_error(Other, Pos),
	?syntax_error(Other, Pos, SyntaxErrorToken)).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec parse(iodata()) -> {ok, t()} | {error, {invalid, non_neg_integer()}} | {error, {invalid, binary(), non_neg_integer()}}.
parse(Input) ->
	parse(Input, #{}).

-spec parse(iodata(), list() | map()) -> {ok, t()} | {error, {invalid, non_neg_integer()}} | {error, {invalid, binary(), non_neg_integer()}}.
parse(Input, Options) when is_binary(Input) andalso is_map(Options) ->
	try
		{Rest0, Pos0} = skip_whitespace(Input, 0),
		{Value, Pos1, Rest1} = value(Rest0, Pos0, maps:get(keys, Options, nil)),
		case skip_whitespace(Rest1, Pos1) of
			{<<>>, _Pos2} ->
				{ok, Value};
			{Other, Pos2} ->
				?syntax_error(Other, Pos2)
		end
	catch
		throw:Throw={invalid, Pos} ->
			_ = Throw,
			{error, {invalid, Pos}};
		throw:Throw={invalid, Token, Pos} ->
			_ = Throw,
			{error, {invalid, Token, Pos}}
	end;
parse(Input, Options) when is_list(Input) ->
	parse(erlang:iolist_to_binary(Input), Options);
parse(Input, Options) when is_list(Options) ->
	parse(Input, maps:from_list(Options)).

-spec 'parse!'(iodata()) -> t() | no_return().
'parse!'(Input) ->
	'parse!'(Input, #{}).

-spec 'parse!'(iodata(), list() | map()) -> t() | no_return().
'parse!'(Input, Options) ->
	case parse(Input, Options) of
		{ok, Value} ->
			Value;
		{error, {invalid, Pos}} ->
			?raise(Pos);
		{error, {invalid, Token, Pos}} ->
			?raise(Pos, Token)
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
value(<< $", Rest/binary >>, Pos, _Keys) ->
	string_continue(Rest, Pos + 1, []);
value(<< ${, Rest0/binary >>, Pos0, Keys) ->
	{Rest1, Pos1} = skip_whitespace(Rest0, Pos0 + 1),
	object_pairs(Rest1, Pos1, Keys, []);
value(<< $[, Rest0/binary >>, Pos0, Keys) ->
	{Rest1, Pos1} = skip_whitespace(Rest0, Pos0 + 1),
	array_values(Rest1, Pos1, Keys, []);
value(<< "null", Rest/binary >>, Pos, _Keys) ->
	{nil, Pos + 4, Rest};
value(<< "true", Rest/binary >>, Pos, _Keys) ->
	{true, Pos + 4, Rest};
value(<< "false", Rest/binary >>, Pos, _Keys) ->
	{false, Pos + 5, Rest};
value(String = << C, _/binary >>, Pos, _Keys) when C == $- orelse (C >= $0 andalso C =< $9) ->
	number_start(String, Pos);
value(Other, Pos, _Keys) ->
	?syntax_error(Other, Pos).

%%% Arrays

%% @private
array_values(<< $], Rest/binary >>, Pos, _, []) ->
	{[], Pos + 1, Rest};
array_values(Rest0, Pos0, Keys, Acc0) ->
	{Value, Pos1, Rest1} = value(Rest0, Pos0, Keys),
	Acc1 = [Value | Acc0],
	case skip_whitespace(Rest1, Pos1) of
		{<< $,, Rest2/binary >>, Pos2} ->
			{Rest3, Pos3} = skip_whitespace(Rest2, Pos2 + 1),
			array_values(Rest3, Pos3, Keys, Acc1);
		{<< $], Rest2/binary >>, Pos2} ->
			{lists:reverse(Acc1), Pos2 + 1, Rest2};
		{Other, Pos2} ->
			?syntax_error(Other, Pos2)
	end.

%%% Numbers

%% @private
number_start(<< $-, Rest0/binary >>, Pos) ->
	case Rest0 of
		<< $0, Rest1/binary >> ->
			number_frac(Rest1, Pos + 2, [<<"-0">>]);
		Rest1 ->
			number_int(Rest1, Pos + 1, [$-])
	end;
number_start(<< $0, Rest/binary >>, Pos) ->
	number_frac(Rest, Pos + 1, [$0]);
number_start(String, Pos) ->
	number_int(String, Pos, []).

%% @private
number_int(String = << C, _/binary >>, Pos0, Acc) when C >= $1 andalso C =< $9 ->
	{Digits, Pos1, Rest} = number_digits(String, Pos0),
	number_frac(Rest, Pos1, [Acc, Digits]);
number_int(Other, Pos, _) ->
	?syntax_error(Other, Pos).

%% @private
number_frac(<< $., Rest0/binary >>, Pos0, Acc) ->
	{Digits, Pos1, Rest1} = number_digits(Rest0, Pos0 + 1),
	number_exp(Rest1, true, Pos1, [Acc, $., Digits]);
number_frac(String, Pos, Acc) ->
	number_exp(String, false, Pos, Acc).

%% @private
number_exp(<< E, Rest0/binary >>, Frac, Pos, Acc) when E == $e orelse E == $E ->
	Exp =
		case Frac of
			true ->
				$e;
			_ ->
				<<".0e">>
		end,
	case Rest0 of
		<< $-, Rest1/binary >> ->
			number_exp_continue(Rest1, Pos + 2, [Acc, Exp, $-]);
		<< $+, Rest1/binary >> ->
			number_exp_continue(Rest1, Pos + 2, [Acc, Exp]);
		Rest1 ->
			number_exp_continue(Rest1, Pos + 1, [Acc, Exp])
	end;
number_exp(String, Frac, Pos, Acc) ->
	{number_complete(Acc, Frac), Pos, String}.

%% @private
number_exp_continue(Rest0, Pos0, Acc) ->
	{Digits, Pos1, Rest1} = number_digits(Rest0, Pos0),
	{number_complete([Acc, Digits], true), Pos1, Rest1}.

%% @private
number_complete(Acc, false) ->
	erlang:binary_to_integer(erlang:iolist_to_binary(Acc));
number_complete(Acc, true) ->
	erlang:binary_to_float(erlang:iolist_to_binary(Acc)).

%% @private
number_digits(String = << C, Rest0/binary >>, Pos) when C >= $0 andalso C =< $9 ->
	Count = number_digits_count(Rest0, 1),
	<< Digits:Count/binary, Rest1/binary >> = String,
	{Digits, Pos + Count, Rest1};
number_digits(Other, Pos) ->
	?syntax_error(Other, Pos).

%% @private
number_digits_count(<< C, Rest/binary >>, Pos) when C >= $0 andalso C =< $9 ->
	number_digits_count(Rest, Pos + 1);
number_digits_count(_, Pos) ->
	Pos.

%%% Objects

%% @private
object_name(Name, atoms) ->
	erlang:binary_to_atom(Name, unicode);
object_name(Name, 'atoms!') ->
	erlang:binary_to_existing_atom(Name, unicode);
object_name(Name, _Keys) ->
	Name.

%% @private
object_pairs(<< $", Rest0/binary >>, Pos0, Keys, Acc0) ->
	{Name, Pos1, Rest1} = string_continue(Rest0, Pos0 + 1, []),
	{Value, Pos4, Rest4} =
		case skip_whitespace(Rest1, Pos1) of
			{<< $:, Rest2/binary >>, Pos2} ->
				{Rest3, Pos3} = skip_whitespace(Rest2, Pos2 + 1),
				value(Rest3, Pos3, Keys);
			{Other0, Pos2} ->
				?syntax_error(Other0, Pos2, SE0)
		end,
	Acc1 = [{object_name(Name, Keys), Value} | Acc0],
	case skip_whitespace(Rest4, Pos4) of
		{<< $,, Rest5/binary >>, Pos5} ->
			{Rest6, Pos6} = skip_whitespace(Rest5, Pos5 + 1),
			object_pairs(Rest6, Pos6, Keys, Acc1);
		{<< $}, Rest5/binary >>, Pos5} ->
			{maps:from_list(Acc1), Pos5 + 1, Rest5};
		{Other1, Pos5} ->
			?syntax_error(Other1, Pos5, SE1)
	end;
object_pairs(<< $}, Rest/binary >>, Pos, _, []) ->
	{maps:new(), Pos + 1, Rest};
object_pairs(Other, Pos, _, _) ->
	?syntax_error(Other, Pos).

%%% Strings

%% @private
string_continue(<< $", Rest/binary >>, Pos, Acc) ->
	{erlang:iolist_to_binary(Acc), Pos + 1, Rest};
string_continue(<< $\\, Rest/binary >>, Pos, Acc) ->
	string_escape(Rest, Pos, Acc);
string_continue(<<>>, Pos, _) ->
	erlang:throw({invalid, Pos});
string_continue(String, Pos0, Acc) ->
	{Count, Pos1} = string_chunk_size(String, Pos0, 0),
	<< Chunk:Count/binary, Rest/binary >> = String,
	string_continue(Rest, Pos1, [Acc, Chunk]).

%% @private
string_escape(<< $", Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $"]);
string_escape(<< $\\, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $\\]);
string_escape(<< $n, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $\n]);
string_escape(<< $t, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $\t]);
string_escape(<< $r, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $\r]);
string_escape(<< $/, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $/]);
string_escape(<< $f, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $\f]);
string_escape(<< $b, Rest/binary >>, Pos, Acc) ->
	string_continue(Rest, Pos + 1, [Acc, $\b]);
string_escape(<< $u, A1, B1, C1, D1, $\\, $u, A2, B2, C2, D2, Rest/binary >>, Pos, Acc)
		when (A1 == $d orelse A1 == $D)
		andalso (A2 == $d orelse A2 == $D)
		andalso (B1 == $8 orelse B1 == $9 orelse B1 == $a orelse B1 == $A orelse B1 == $b orelse B1 == $B)
		andalso ((B2 >= $c andalso B2 =< $f) orelse (B2 >= $C andalso B2 =< $F))
		andalso ((C1 >= $0 andalso C1 =< $9) orelse (C1 >= $a andalso C1 =< $f) orelse (C1 >= $A andalso C1 =< $F))
		andalso ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f) orelse (D1 >= $A andalso D1 =< $F))
		andalso ((C2 >= $0 andalso C2 =< $9) orelse (C2 >= $a andalso C2 =< $f) orelse (C2 >= $A andalso C2 =< $F))
		andalso ((D2 >= $0 andalso D2 =< $9) orelse (D2 >= $a andalso D2 =< $f) orelse (D2 >= $A andalso D2 =< $F)) ->
	Hi = erlang:binary_to_integer(<< A1, B1, C1, D1 >>, 16),
	Lo = erlang:binary_to_integer(<< A2, B2, C2, D2 >>, 16),
	Codepoint = 16#10000 + ((Hi band 16#03FF) bsl 10) + (Lo band 16#03FF),
	Token =
		try
			<< Codepoint/utf8 >>
		catch
			_:_ ->
				?syntax_error(<< $u >>, Pos)
		end,
	string_continue(Rest, Pos + 11, [Acc, Token]);
string_escape(<< $u, A1, B1, C1, D1, Rest/binary >>, Pos, Acc)
		when ((A1 >= $0 andalso A1 =< $9) orelse (A1 >= $a andalso A1 =< $f) orelse (A1 >= $A andalso A1 =< $F))
		andalso ((B1 >= $0 andalso B1 =< $9) orelse (B1 >= $a andalso B1 =< $f) orelse (B1 >= $A andalso B1 =< $F))
		andalso ((C1 >= $0 andalso C1 =< $9) orelse (C1 >= $a andalso C1 =< $f) orelse (C1 >= $A andalso C1 =< $F))
		andalso ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f) orelse (D1 >= $A andalso D1 =< $F)) ->
	Codepoint = erlang:binary_to_integer(<< A1, B1, C1, D1 >>, 16),
	Token =
		try
			<< Codepoint/utf8 >>
		catch
			_:_ ->
				?syntax_error(<< $u >>, Pos)
		end,
	string_continue(Rest, Pos + 5, [Acc, Token]);
string_escape(Other, Pos, _) ->
	?syntax_error(Other, Pos).

%% @private
string_chunk_size(<< $", _/binary >>, Pos, Acc) ->
	{Acc, Pos};
string_chunk_size(<< $\\, _/binary >>, Pos, Acc) ->
	{Acc, Pos};
string_chunk_size(<< C, Rest/binary >>, Pos, Acc) when C < 16#80 ->
	string_chunk_size(Rest, Pos + 1, Acc + 1);
string_chunk_size(<< C/utf8, Rest/binary >>, Pos, Acc) ->
	string_chunk_size(Rest, Pos + 1, Acc + string_codepoint_size(C));
string_chunk_size(Other, Pos, _) ->
	?syntax_error(Other, Pos).

%% @private
string_codepoint_size(C) when C < 16#800 ->
	2;
string_codepoint_size(C) when C < 16#10000 ->
	3;
string_codepoint_size(_) ->
	4.

%%% Whitespace

%% @private
skip_whitespace(<< C, Rest/binary >>, Pos) when C == $\s orelse C == $\n orelse C == $\t orelse C == $\r ->
	skip_whitespace(Rest, Pos + 1);
skip_whitespace(String, Pos) ->
	{String, Pos}.
