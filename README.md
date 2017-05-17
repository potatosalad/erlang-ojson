# Ordered JSON (OJSON)

[![Build Status](https://travis-ci.org/potatosalad/erlang-ojson.svg?branch=master)](https://travis-ci.org/potatosalad/erlang-ojson) [![Hex.pm](https://img.shields.io/hexpm/v/ojson.svg)](https://hex.pm/packages/ojson)

Ordered JSON (JSON) is a deterministic or stable JSON encoder and decoder for Erlang and Elixir.

OJSON is similar to projects like [fast-stable-stringify](https://github.com/nickyout/fast-stable-stringify) and [json-stable-stringify](https://github.com/substack/json-stable-stringify) which allows for deterministic serialization of object key order.

Before you say, ["JSON is unordered!"](https://tools.ietf.org/html/rfc7159#section-1), the initial use-case for this library is with cryptographic operations, which rely on stable input for authentication and verification in serialized form.  The ordering only applies to the encoding/serialization operations.

Initially based on [devinus/poison](https://github.com/devinus/poison), but rewritten for use as a dependency for Erlang or Elixir projects.

## Installation

If [using Hex](https://hex.pm/), the package can be installed
by adding `ojson` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:ojson, "~> 0.0.0"}]
end
```

If [using rebar3](http://www.rebar3.org/), the package can be installed by adding `ojson` to you list of dependencies in `rebar.config`:

```erlang
{deps, [
  {ojson, ".*", {git, "git://github.com/potatosalad/erlang-ojson.git", {branch, "master"}}}
]}.
```

The [HexDocs](https://hexdocs.pm) can
be found at [https://hexdocs.pm/ojson](https://hexdocs.pm/ojson).

## Usage

##### Basic Usage

_Elixir_

```elixir
iex> OJSON.encode(%{"z" => 1, "a" => 2})
{:ok, "{\"a\":2,\"z\":1}"}

iex> OJSON.encode!(%{"z" => 1, "a" => 2})
"{\"a\":2,\"z\":1}"

iex> OJSON.encode_to_iodata(%{"z" => 1, "a" => 2})
{:ok, [123, [[34, ["a"], 34], 58, "2"], [[44, [34, ["z"], 34], 58, "1"]], 125]}

iex> OJSON.encode_to_iodata!(%{"z" => 1, "a" => 2})
[123, [[34, ["a"], 34], 58, "2"], [[44, [34, ["z"], 34], 58, "1"]], 125]

iex> OJSON.decode("{\"a\":2,\"z\":1}")
{:ok, %{"a" => 2, "z" => 1}}

iex> OJSON.decode!("{\"a\":2,\"z\":1}")
%{"a" => 2, "z" => 1}
```

_Erlang_

```erlang
ojson:encode(#{ <<"z">> => 1, <<"a">> => 2 }).
% {ok, <<"{\"a\":2,\"z\":1}">>}

ojson:'encode!'(#{ <<"z">> => 1, <<"a">> => 2 }).
% <<"{\"a\":2,\"z\":1}">>

ojson:encode_to_iodata(#{ <<"z">> => 1, <<"a">> => 2 }).
% {ok, [123,[[34,[<<"a">>],34],58,<<"2">>],[[44,[34,[<<"z">>],34],58,<<"1">>]],125]}

ojson:'encode_to_iodata!'(#{ <<"z">> => 1, <<"a">> => 2 }).
% [123,[[34,[<<"a">>],34],58,<<"2">>],[[44,[34,[<<"z">>],34],58,<<"1">>]],125]

ojson:decode(<<"{\"a\":2,\"z\":1}">>).
% {ok, #{<<"a">> => 2, <<"z">> => 1}}

ojson:'decode!'(<<"{\"a\":2,\"z\":1}">>).
% #{<<"a">> => 2, <<"z">> => 1}
```

##### Deterministic Example

_Elixir_

```elixir
# Let's first generate a map with 32 key/value pairs:
iex> map = for key <- :lists.seq(0, 32), into: %{}, do: {String.pad_leading(:erlang.integer_to_binary(key, 16), 2, "0"), 0}
%{"10" => 0, "0E" => 0, "14" => 0, "12" => 0, "16" => 0, "07" => 0, "1C" => 0,
  "06" => 0, "19" => 0, "13" => 0, "04" => 0, "1E" => 0, "1F" => 0, "11" => 0,
  "15" => 0, "17" => 0, "1A" => 0, "0A" => 0, "18" => 0, "0C" => 0, "00" => 0,
  "0B" => 0, "09" => 0, "02" => 0, "0D" => 0, "1D" => 0, "20" => 0, "03" => 0,
  "0F" => 0, "1B" => 0, "01" => 0, "05" => 0, "08" => 0}

# Notice that the key/value pairs are unordered.
# Now let's encode our unordered map into an ordered JSON string:
iex> OJSON.encode!(map)
"{\"00\":0,\"01\":0,\"02\":0,\"03\":0,\"04\":0,\"05\":0,\"06\":0,\"07\":0,\"08\":0,\"09\":0,\"0A\":0,\"0B\":0,\"0C\":0,\"0D\":0,\"0E\":0,\"0F\":0,\"10\":0,\"11\":0,\"12\":0,\"13\":0,\"14\":0,\"15\":0,\"16\":0,\"17\":0,\"18\":0,\"19\":0,\"1A\":0,\"1B\":0,\"1C\":0,\"1D\":0,\"1E\":0,\"1F\":0,\"20\":0}"
```

_Erlang_

```erlang
%% Let's first generate a map with 32 key/value pairs:
Map = maps:from_list([{case erlang:integer_to_binary(I, 16) of <<B>> -> <<$0,B>>; B -> B end, 0} || I <- lists:seq(0, 32)]).
% #{<<"10">> => 0,<<"0E">> => 0,<<"14">> => 0,<<"12">> => 0,
%   <<"16">> => 0,<<"07">> => 0,<<"1C">> => 0,<<"06">> => 0,
%   <<"19">> => 0,<<"13">> => 0,<<"04">> => 0,<<"1E">> => 0,
%   <<"1F">> => 0,<<"11">> => 0,<<"15">> => 0,<<"17">> => 0,
%   <<"1A">> => 0,<<"0A">> => 0,<<"18">> => 0,<<"0C">> => 0,
%   <<"00">> => 0,<<"0B">> => 0,<<"09">> => 0,<<"02">> => 0,
%   <<"0D">> => 0,<<"1D">> => 0,<<"20">> => 0,<<"03">> => 0,
%   <<"0F">> => 0,<<"1B">> => 0,<<"01">> => 0,<<"05">> => 0,
%   <<"08">> => 0}

%% Notice that the key/value pairs are unordered.
%% Now let's encode our unordered map into an ordered JSON string:
ojson:'encode!'(Map).
% <<"{\"00\":0,\"01\":0,\"02\":0,\"03\":0,\"04\":0,\"05\":0,\"06\":0,\"07\":0,\"08\":0,\"09\":0,\"0A\":0,\"0B\":0,\"0C\":0,\"0D\":0,\"0E\":0,\"0F\":0,\"10\":0,\"11\":0,\"12\":0,\"13\":0,\"14\":0,\"15\":0,\"16\":0,\"17\":0,\"18\":0,\"19\":0,\"1A\":0,\"1B\":0,\"1C\":0,\"1D\":0,\"1E\":0,\"1F\":0,\"20\":0}">>
```
