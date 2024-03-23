defmodule Jsont.NifBridge do
  use Rustler,
    otp_app: :jsont,
    crate: "jsont_nif"

  @spec encode(term(), boolean(), boolean()) :: {:ok, String.t()} | {:error, any()}
  def encode(_value, _bigint_as_string, _skip_elixir_struct),
    do: :erlang.nif_error(:nif_not_loaded)

  @spec decode(iodata(), boolean()) :: {:ok, term()} | {:error, any()}
  def decode(_value, _validate_unicode), do: :erlang.nif_error(:nif_not_loaded)
end
