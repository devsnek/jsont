decode_jobs = %{
  "Jsont" => fn {json, _} -> {:ok, _} = Jsont.decode(json) end,
  "jiffy" => fn {json, _} -> :jiffy.decode(json, [:return_maps, :use_nil]) end,
  "binary_to_term/1" => fn {_, etf} -> :erlang.binary_to_term(etf) end
}

decode_inputs = [
  "GitHub",
  "Giphy",
  "GovTrack",
  "Blockchain",
  "Pokedex",
  "JSON Generator",
  "JSON Generator (Pretty)",
  "UTF-8 escaped",
  "UTF-8 unescaped",
  "Issue 90"
]

read_data = fn name ->
  file =
    name
    |> String.downcase()
    |> String.replace(~r/([^\w]|-|_)+/, "-")
    |> String.trim("-")

  json = File.read!(Path.expand("data/#{file}.json", __DIR__))
  {:ok, term} = Jsont.decode(json)
  etf = :erlang.term_to_binary(term)

  {json, etf}
end

inputs = for name <- decode_inputs, into: %{}, do: {name, read_data.(name)}

IO.puts("Checking jobs don't crash")

for {name, input} <- inputs, {job, decode_job} <- decode_jobs do
  IO.puts("Testing #{job} #{name}")
  decode_job.(input)
end

IO.puts("\n")

Benchee.run(decode_jobs,
  warmup: 5,
  time: 10,
  memory_time: 0.01,
  inputs: inputs,
  save: %{path: "output/runs/#{DateTime.utc_now()}.benchee"},
  load: "output/runs/*.benchee",
  formatters: [
    {Benchee.Formatters.HTML, file: Path.expand("output/decode.html", __DIR__)},
    Benchee.Formatters.Console
  ]
)
