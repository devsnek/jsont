encode_jobs = %{
  "Jsont" => fn value ->
    {:ok, _} = Jsont.encode(value)
  end,
  "jiffy" => &:jiffy.encode/1,
  "term_to_binary" => &:erlang.term_to_binary/1
}

encode_inputs = [
  "GitHub",
  "Giphy",
  "GovTrack",
  "Blockchain",
  "Pokedex",
  "JSON Generator",
  "UTF-8 unescaped",
  "Issue 90",
  "Canada"
]

read_data = fn name ->
  name
  |> String.downcase()
  |> String.replace(~r/([^\w]|-|_)+/, "-")
  |> String.trim("-")
  |> (&"data/#{&1}.json").()
  |> Path.expand(__DIR__)
  |> File.read!()
end

Benchee.run(encode_jobs,
  warmup: 2,
  time: 10,
  memory_time: 0.01,
  reduction_time: 0.01,
  inputs:
    for name <- encode_inputs, into: %{} do
      {:ok, data} = Jsont.decode(read_data.(name))
      {name, data}
    end,
  formatters: [
    {Benchee.Formatters.HTML, file: Path.expand("output/encode.html", __DIR__)},
  ]
)
