defmodule Jsont do
  @type encode_opt :: {:bigint_as_string, boolean()}

  @spec encode(term(), [encode_opt()]) :: {:ok, String.t()} | {:error, any()}
  def encode(value, opts \\ []) do
    bigint_as_string = opts[:bigint_as_string] || false
    Jsont.NifBridge.encode(value, bigint_as_string)
  end

  @spec decode(iodata()) :: {:ok, term()} | {:error, any()}
  def decode(value) do
    Jsont.NifBridge.decode(value)
  end
end
