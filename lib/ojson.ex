defmodule OJSON do

  @doc """
  Decode JSON to a value.
      iex> OJSON.decode("[1,2,3]")
      {:ok, [1, 2, 3]}
  """
  @spec decode(iodata(), :ojson.decoder_options()) :: {:ok, :ojson_decoder.t()} | {:error, {:invalid, non_neg_integer()}} | {:error, {:invalid, binary(), non_neg_integer()}}
  def decode(iodata, options \\ %{}) do
    :ojson.decode(iodata, options)
  end

  @doc """
  Decode JSON to a value, raises an exception on error.
      iex> OJSON.decode!("[1,2,3]")
      [1, 2, 3]
  """
  @spec decode!(iodata(), :ojson.decoder_options()) :: :ojson_decoder.t() | no_return()
  def decode!(iodata, options \\ %{}) do
    :ojson.decode!(iodata, options)
  end

  # @doc """
  # Encode a value to JSON.
  #     iex> OJSON.encode([1, 2, 3])
  #     {:ok, "[1,2,3]"}
  # """
  @spec encode(term(), :ojson.encoder_options()) :: {:ok, iodata()} | {:ok, binary()} | {:error, {:invalid, any()}}
  def encode(value, options \\ %{}) do
    :ojson.encode(value, options)
  end

  @doc """
  Encode a value to JSON, raises an exception on error.
      iex> OJSON.encode!([1, 2, 3])
      "[1,2,3]"
  """
  @spec encode!(term(), :ojson.encoder_options()) :: iodata() | binary() | no_return()
  def encode!(value, options \\ %{}) do
    :ojson.encode!(value, options)
  end

  @doc """
  Encode a value to JSON as iodata.
      iex> OJSON.encode_to_iodata([1, 2, 3])
      {:ok, [91, ["1"], [[44, "2"], [44, "3"]], 93]}
  """
  @spec encode_to_iodata(term(), :ojson.encoder_options()) :: {:ok, iodata()} | {:error, {:invalid, any()}}
  def encode_to_iodata(value, options \\ %{}) do
    :ojson.encode_to_iodata(value, options)
  end

  @doc """
  Encode a value to JSON as iodata, raises an exception on error.
      iex> OJSON.encode_to_iodata!([1, 2, 3])
      [91, ["1"], [[44, "2"], [44, "3"]], 93]
  """
  @spec encode_to_iodata!(term(), :ojson.encoder_options()) :: iodata() | no_return()
  def encode_to_iodata!(value, options \\ %{}) do
    :ojson.encode_to_iodata!(value, options)
  end

end