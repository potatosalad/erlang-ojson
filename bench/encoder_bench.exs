defmodule EncoderBench do
  use Benchfella

  # Lists
  bench "lists (OJSON)", [list: gen_list()] do
    OJSON.encode!(list)
  end

  bench "lists (Poison)", [list: gen_list()] do
    Poison.encode!(list)
  end

  bench "lists (jiffy)", [list: gen_list()] do
    :jiffy.encode(list)
  end

  bench "lists (JSX)", [list: gen_list()] do
    JSX.encode!(list)
  end

  bench "lists (JSON)", [list: gen_list()] do
    JSON.encode!(list)
  end

  bench "lists (Jazz)", [list: gen_list()] do
    Jazz.encode!(list)
  end

  bench "lists (jsone)", [list: gen_list()] do
    :jsone.encode(list)
  end

  # Maps
  bench "maps (OJSON)", [map: gen_map()] do
    OJSON.encode!(map)
  end

  bench "maps (Poison)", [map: gen_map()] do
    Poison.encode!(map)
  end

  bench "maps (jiffy)", [map: gen_map()] do
    :jiffy.encode(map)
  end

  bench "maps (JSX)", [map: gen_map()] do
    JSX.encode!(map)
  end

  bench "maps (JSON)", [map: gen_map()] do
    JSON.encode!(map)
  end

  bench "maps (Jazz)", [map: gen_map()] do
    Jazz.encode!(map)
  end

  bench "maps (jsone)", [map: gen_map()] do
    :jsone.encode(map)
  end

  # Strings
  bench "strings (OJSON)", [string: gen_string()] do
    OJSON.encode!(string)
  end

  bench "strings (Poison)", [string: gen_string()] do
    Poison.encode!(string)
  end

  bench "strings (jiffy)", [string: gen_string()] do
    :jiffy.encode(string)
  end

  bench "strings (JSX)", [string: gen_string()] do
    JSX.encode!(string)
  end

  bench "strings (JSON)", [string: gen_string()] do
    JSON.encode!(string)
  end

  bench "strings (Jazz)", [string: gen_string()] do
    Jazz.encode!(string)
  end

  bench "strings (jsone)", [string: gen_string()] do
    :jsone.encode(string, [:native_utf8])
  end

  # String escaping
  bench "string escaping (OJSON)", [string: gen_string()] do
    OJSON.encode!(string, escape: :unicode)
  end

  bench "string escaping (Poison)", [string: gen_string()] do
    Poison.encode!(string, escape: :unicode)
  end

  bench "string escaping (jiffy)", [string: gen_string()] do
    :jiffy.encode(string, [:uescape])
  end

  bench "string escaping (JSX)", [string: gen_string()] do
    JSX.encode!(string, [:uescape])
  end

  bench "string escaping (Jazz)", [string: gen_string()] do
    Jazz.encode!(string, escape: :unicode)
  end

  bench "string escaping (jsone)", [string: gen_string()] do
    :jsone.encode(string)
  end

  # Structs
  bench "structs (OJSON)", [structs: gen_structs()] do
    OJSON.encode!(structs)
  end

  bench "structs (Poison)", [structs: gen_structs()] do
    Poison.encode!(structs)
  end

  bench "structs (JSX)", [structs: gen_structs()] do
    JSX.encode!(structs)
  end

  bench "structs (JSON)", [structs: gen_structs()] do
    JSON.encode!(structs)
  end

  bench "structs (Jazz)", [structs: gen_structs()] do
    Jazz.encode!(structs)
  end

  bench "OJSON", [data: gen_data()] do
    OJSON.encode!(data)
  end

  bench "Poison", [data: gen_data()] do
    Poison.encode!(data)
  end

  bench "jiffy", [data: gen_data()] do
    :jiffy.encode(data)
  end

  bench "JSX", [data: gen_data()] do
    JSX.encode!(data)
  end

  bench "JSON", [data: gen_data()] do
    JSON.encode!(data)
  end

  bench "Jazz", [data: gen_data()] do
    Jazz.encode!(data)
  end

  bench "jsone", [data: gen_data()] do
    :jsone.encode(data)
  end

  bench "OJSON (pretty)", [data: gen_data()] do
    OJSON.encode!(data, pretty: true)
  end

  bench "Poison (pretty)", [data: gen_data()] do
    Poison.encode!(data, pretty: true)
  end

  bench "jiffy (pretty)", [data: gen_data()] do
    :jiffy.encode(data, [:pretty])
  end

  bench "JSX (pretty)", [data: gen_data()] do
    JSX.encode!(data) |> JSX.prettify!()
  end

  bench "Jazz (pretty)", [data: gen_data()] do
    Jazz.encode!(data, pretty: true)
  end

  bench "jsone (pretty)", [data: gen_data()] do
    :jsone.encode(data, [{:indent, 1}, {:space, 2}])
  end

  defp gen_list() do
    Enum.to_list(1..1000)
  end

  defp gen_map() do
    Stream.map(?A..?Z, &<<&1>>) |> Stream.with_index() |> Enum.into(%{})
  end

  defp gen_string() do
    Path.expand("data/UTF-8-demo.txt", __DIR__) |> File.read!()
  end

  defmodule Struct do
    defstruct x: nil
  end

  defp gen_structs() do
    1..10 |> Enum.map(&(%Struct{x: &1}))
  end

  defp gen_data() do
    Path.expand("data/generated.json", __DIR__) |> File.read!() |> OJSON.decode!()
  end
end