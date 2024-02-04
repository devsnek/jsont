defmodule JsontBench.MixProject do
  use Mix.Project

  def project do
    [
      app: :jsont_bench,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp aliases() do
    [
      "bench.encode": ["run encode.exs"],
      "bench.decode": ["run decode.exs"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:jsont, "~> 0.1", path: "../", override: true},
      {:benchee, "~> 1.0"},
      {:benchee_html, "~> 1.0"},
      {:jiffy, "~> 1.1"}
    ]
  end
end
