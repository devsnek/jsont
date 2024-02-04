defmodule Jsont.MixProject do
  use Mix.Project

  def project do
    [
      app: :jsont,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:rustler, "~> 0.30.0", runtime: false},
      {:stream_data, "~> 0.5", only: [:dev, :test], runtime: false},
      {:snapshy, "~> 0.4", only: [:dev, :test], runtime: false}
    ]
  end
end
