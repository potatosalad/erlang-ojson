defmodule OJSON.EncoderTest do
  use ExUnit.Case, async: true

  alias OJSON.EncodeError

  test "Atom" do
    assert to_json(nil) == "null"
    assert to_json(true) == "true"
    assert to_json(false) == "false"
    assert to_json(:OJSON) == ~s("OJSON")
  end

  test "Integer" do
    assert to_json(42) == "42"
  end

  test "Float" do
    assert to_json(99.99) == "99.99"
    assert to_json(9.9e100) == "9.9e100"
  end

  test "BitString" do
    assert to_json("hello world") == ~s("hello world")
    assert to_json("hello\nworld") == ~s("hello\\nworld")
    assert to_json("\nhello\nworld\n") == ~s("\\nhello\\nworld\\n")

    assert to_json("\"") == ~s("\\"")
    assert to_json("\0") == ~s("\\u0000")
    assert to_json(<<31>>) == ~s("\\u001F")
    assert to_json("☃", escape: :unicode) == ~s("\\u2603")
    assert to_json("𝄞", escape: :unicode) == ~s("\\uD834\\uDD1E")
    assert to_json("\u2028\u2029", escape: :javascript) == ~s("\\u2028\\u2029")
    assert to_json("</script>", escape: :html_safe) == ~s("<\\/script>")
    assert to_json(~s(<script>var s = "\u2028\u2029";</script>), escape: :html_safe) == ~s("<script>var s = \\\"\\u2028\\u2029\\\";<\\/script>")
    assert to_json("áéíóúàèìòùâêîôûãẽĩõũ") == ~s("áéíóúàèìòùâêîôûãẽĩõũ")
  end

  test "Map" do
    assert to_json(%{}) == "{}"
    assert to_json(%{"foo" => "bar"})  == ~s({"foo":"bar"})
    assert to_json(%{foo: :bar}) == ~s({"foo":"bar"})
    assert to_json(%{42 => :bar}) == ~s({"42":"bar"})
    assert to_json(%{'foo' => :bar}) == ~s({"foo":"bar"})
    assert to_json(%{foo: %{bar: %{baz: "baz"}}}, pretty: true) == """
    {
      "foo": {
        "bar": {
          "baz": "baz"
        }
      }
    }\
    """

    multi_key_map = %{"foo" => "foo1", :foo => "foo2"}
    assert to_json(multi_key_map) == ~s({"foo":"foo1"})
    assert OJSON.encode(multi_key_map, strict_keys: true) == {:error, {:invalid, "foo"}}
  end

  test "List" do
    assert to_json([]) == "[]"
    assert to_json([1, 2, 3]) == "[1,2,3]"
    assert to_json([1, 2, 3], pretty: true) == """
    [
      1,
      2,
      3
    ]\
    """
  end

  test "Range" do
    assert to_json(1..3) == "[1,2,3]"
    assert to_json(1..3, pretty: true) == """
    [
      1,
      2,
      3
    ]\
    """
  end

  test "Stream" do
    range = 1..10
    assert to_json(Stream.take(range, 0)) == "[]"
    assert to_json(Stream.take(range, 3)) == "[1,2,3]"
    assert to_json(Stream.take(range, 3), pretty: true) == """
    [
      1,
      2,
      3
    ]\
    """
  end

  # MapSet/HashSet/HashDict have an unspecified order

  test "MapSet/HashSet" do
    for type <- [MapSet, HashSet] do
      set = type.new
      assert to_json(set) == "[]"

      set = set |> type.put(1) |> type.put(2)

      assert to_json(set) in ~w([1,2] [2,1])
      assert to_json(set, pretty: true) in [
        """
        [
          1,
          2
        ]\
        """,
        """
        [
          2,
          1
        ]\
        """
      ]
    end
  end

  if Application.get_env(:OJSON, :enable_hashdict) do
    test "HashDict" do
      dict = HashDict.new
      assert to_json(dict) == "{}"

      dict = dict |> HashDict.put(:foo, "bar") |> HashDict.put(:baz, "quux")

      assert to_json(dict) in ~w"""
      {"foo":"bar","baz":"quux"}
      {"baz":"quux","foo":"bar"}
      """

      assert to_json(dict, pretty: true) in [
        """
        {
          "foo": "bar",
          "baz": "quux"
        }\
        """,
        """
        {
          "baz": "quux",
          "foo": "bar"
        }\
        """
      ]
    end
  end

  if Version.match?(System.version, ">=1.3.0-rc.1") do
    test "Time" do
      {:ok, time} = Time.new(12, 13, 14)
      assert to_json(time) == ~s("12:13:14")
    end

    test "Date" do
      {:ok, date} = Date.new(2000, 1, 1)
      assert to_json(date) == ~s("2000-01-01")
    end

    test "NaiveDateTime" do
      {:ok, datetime} = NaiveDateTime.new(2000, 1, 1, 12, 13, 14)
      assert to_json(datetime) == ~s("2000-01-01T12:13:14")
    end

    test "DateTime" do
      datetime = %DateTime{year: 2000, month: 1, day: 1, hour: 12, minute: 13, second: 14,
                           microsecond: {0, 0}, zone_abbr: "CET", time_zone: "Europe/Warsaw",
                           std_offset: -1800, utc_offset: 3600}
      assert to_json(datetime) == ~s("2000-01-01T12:13:14+00:30")

      datetime = %DateTime{year: 2000, month: 1, day: 1, hour: 12, minute: 13, second: 14,
                           microsecond: {50000, 3}, zone_abbr: "UTC", time_zone: "Etc/UTC",
                           std_offset: 0, utc_offset: 0}
      assert to_json(datetime) == ~s("2000-01-01T12:13:14.050Z")
    end
  end

  test "EncodeError" do
    assert_raise EncodeError, fn ->
      to_json(self())
    end

    assert_raise EncodeError, fn ->
      assert to_json(<<0x80>>)
    end
  end

  defp to_json(value, options \\ []) do
    options = :maps.from_list(options)
    OJSON.Encoder.encode(value, options) |> IO.iodata_to_binary
  end
end