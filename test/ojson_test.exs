defmodule OJSONTest do
  use ExUnit.Case, async: true
  use Quixir

  doctest OJSON

  test "roundtrip atom" do
    ptest [input: atom()] do
      roundtrip(input, Atom.to_string(input))
    end
  end

  test "roundtrip boolean" do
    ptest [input: bool()] do
      roundtrip(input)
    end
  end

  test "roundtrip float" do
    ptest [input: float()] do
      roundtrip(input)
    end
  end

  test "roundtrip integer" do
    ptest [input: int()] do
      roundtrip(input)
    end
  end

  test "roundtrip list" do
    ptest [input: list(of: choose(from: [bool(), float(), int(), string()]))] do
      roundtrip(input)
    end
  end

  test "roundtrip map" do
    ptest [input: map(of: [{string(), choose(from: [bool(), float(), int(), string()])}])] do
      roundtrip(input)
    end
  end

  test "roundtrip string (ascii)" do
    ptest [input: string(chars: :ascii)] do
      roundtrip(input)
    end
  end

  test "roundtrip string (binary)" do
    ptest [input: string(chars: 0x00..0xff)] do
      roundtrip(input)
    end
  end

  test "roundtrip string (digits)" do
    ptest [input: string(chars: :digits)] do
      roundtrip(input)
    end
  end

  test "roundtrip string (lower)" do
    ptest [input: string(chars: :lower)] do
      roundtrip(input)
    end
  end

  test "roundtrip string (printable)" do
    ptest [input: string(chars: :printable)] do
      roundtrip(input)
    end
  end

  test "roundtrip string (upper)" do
    ptest [input: string(chars: :upper)] do
      roundtrip(input)
    end
  end

  test "roundtrip string (utf)" do
    ptest [input: string(chars: :utf)] do
      roundtrip(input)
    end
  end

  @doc false
  defp roundtrip(input, compare \\ nil) do
    compare = if compare, do: compare, else: input
    output = OJSON.encode!(input)
    assert OJSON.decode!(output) == compare
    output = OJSON.encode!(input, escape: :html_safe)
    assert OJSON.decode!(output) == compare
    output = OJSON.encode!(input, escape: :javascript)
    assert OJSON.decode!(output) == compare
    output = OJSON.encode!(input, escape: :unicode)
    assert OJSON.decode!(output) == compare
  end
end