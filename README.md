# Json't

Json't is a JSON encoder/decoder for Elixir. Building on the incredible
work of [`serde_json`][] and [V8][], Json't is able to achieve correct,
lightning fast results.

```elixir
{:ok, bin} = Jsont.encode("Hello, world!")
```

```elixir
{:ok, term} = Jsont.decode(~s("Hello, world!"))
```

[`serde_json`]: https://github.com/serde-rs/json
[V8]: https://v8.dev/
