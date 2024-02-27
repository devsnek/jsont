defmodule Jsont do
  @type encode_opt :: {:bigint_as_string, boolean()} | {:strip_elixir_struct, boolean()}

  @spec encode(term(), [encode_opt()]) :: {:ok, String.t()} | {:error, any()}
  def encode(value, opts \\ []) do
    bigint_as_string = opts[:bigint_as_string] || false
    strip_elixir_struct = opts[:strip_elixir_struct] || false
    Jsont.NifBridge.encode(value, bigint_as_string, strip_elixir_struct)
  end

  @spec decode(iodata()) :: {:ok, term()} | {:error, any()}
  def decode(value) do
    Jsont.NifBridge.decode(value)
  end
end
