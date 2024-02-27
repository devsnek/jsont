defmodule Jsont.NifBridge do
  use Rustler,
    otp_app: :jsont,
    crate: "jsont_nif"

  @spec encode(term(), boolean(), boolean()) :: {:ok, String.t()} | {:error, any()}
  def encode(_value, _bigint_as_string, _skip_elixir_struct),
    do: :erlang.nif_error(:nif_not_loaded)

  @spec decode(iodata()) :: {:ok, term()} | {:error, any()}
  def decode(_value), do: :erlang.nif_error(:nif_not_loaded)
end
