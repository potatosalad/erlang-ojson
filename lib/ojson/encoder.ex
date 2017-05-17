defprotocol OJSON.Encoder do
  @fallback_to_any true

  def encode(value, options)
end

defimpl OJSON.Encoder, for: Atom do
  defdelegate encode(value, options), to: :ojson_encoder, as: :encode_atom
end

defimpl OJSON.Encoder, for: BitString do
  defdelegate encode(value, options), to: :ojson_encoder, as: :encode_bitstring
end

defimpl OJSON.Encoder, for: Float do
  defdelegate encode(value, options), to: :ojson_encoder, as: :encode_float
end

defimpl OJSON.Encoder, for: Integer do
  defdelegate encode(value, options), to: :ojson_encoder, as: :encode_integer
end

defimpl OJSON.Encoder, for: List do
  defdelegate encode(value, options), to: :ojson_encoder, as: :encode_list
end

defimpl OJSON.Encoder, for: Map do
  defdelegate encode(value, options), to: :ojson_encoder, as: :encode_map
end

defimpl OJSON.Encoder, for: [Range, Stream, MapSet, HashSet] do
  def encode(collection, options) do
    encode(collection, :maps.get(:pretty, options, false), options)
  end

  @doc false
  defp encode(collection, true, options) do
    indent = :maps.get(:indent, options, 2)
    offset = :maps.get(:offset, options, 0) + indent
    options = :maps.put(:offset, offset, options)

    fun = &[",\n", :binary.copy(" ", offset), OJSON.Encoder.encode(&1, options)]

    case Enum.flat_map(collection, fun) do
      [] -> "[]"
      [_ | tail] -> ["[\n", tail, ?\n, :binary.copy(" ", offset - indent), ?]]
    end
  end
  defp encode(collection, _, options) do
    fun = &[?,, OJSON.Encoder.encode(&1, options)]

    case Enum.flat_map(collection, fun) do
      [] -> "[]"
      [_ | tail] -> [?[, tail, ?]]
    end
  end
end

defimpl OJSON.Encoder, for: [Date, Time, NaiveDateTime, DateTime] do
  def encode(value, options) do
    OJSON.Encoder.BitString.encode(@for.to_iso8601(value), options)
  end
end

defimpl OJSON.Encoder, for: Any do
  def encode(%{__struct__: _} = struct, options) do
    OJSON.Encoder.Map.encode(Map.from_struct(struct), options)
  end

  def encode(value, _options) do
    raise OJSON.EncodeError, value: value
  end
end
