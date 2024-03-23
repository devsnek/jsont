defmodule Jsont.EncoderTest do
  use ExUnit.Case, async: true

  defmodule Struct do
    defstruct field: "hi"
  end

  test "atom" do
    assert to_json(nil) == "null"
    assert to_json(true) == "true"
    assert to_json(false) == "false"
    assert to_json(:something) == ~s("something")
  end

  test "integer" do
    assert to_json(42) == "42"
    assert to_json(2 ** 63 - 1) == "9223372036854775807"
    assert to_json(2 ** 64) == "18446744073709551616"
    assert to_json(-(2 ** 64)) == "-18446744073709551616"
    assert to_json(2 ** 100) == "1267650600228229401496703205376"
    assert to_json(-(2 ** 100)) == "-1267650600228229401496703205376"

    assert to_json(42, bigint_as_string: true) == "42"
    assert to_json(2 ** 63 - 1, bigint_as_string: true) == ~s("9223372036854775807")
    assert to_json(2 ** 64, bigint_as_string: true) == ~s("18446744073709551616")
    assert to_json(-(2 ** 64), bigint_as_string: true) == ~s("-18446744073709551616")
    assert to_json(2 ** 100, bigint_as_string: true) == ~s("1267650600228229401496703205376")
    assert to_json(-(2 ** 100), bigint_as_string: true) == ~s("-1267650600228229401496703205376")
  end

  test "float" do
    assert to_json(99.99) == "99.99"
    assert to_json(9.9e100) == "9.9e100"
    assert to_json(99.99, bigint_as_string: true) == "99.99"
    assert to_json(9.9e100, bigint_as_string: true) == "9.9e100"
  end

  test "binary" do
    assert to_json("hello world") == ~s("hello world")
    assert to_json("hello\nworld") == ~s("hello\\nworld")
    assert to_json("\nhello\nworld\n") == ~s("\\nhello\\nworld\\n")

    assert to_json("\"") == ~s("\\"")
    assert to_json("\0") == ~s("\\u0000")
    assert to_json(<<31>>) == ~s("\\u001f")
    assert to_json(<<255>>) == ~s("\\u00ff")
    assert to_json("☃a") == ~s("☃a")
    assert to_json("𝄞b") == ~s("𝄞b")
    assert to_json("áéíóúàèìòùâêîôûãẽĩõũ") == ~s("áéíóúàèìòùâêîôûãẽĩõũ")
  end

  test "map" do
    assert to_json(%{}) == "{}"
    assert to_json(%{"foo" => "bar"}) == ~s({"foo":"bar"})
    assert to_json(%{foo: :bar}) == ~s({"foo":"bar"})
    assert to_json(%{"foo" => "foo1", :foo => "foo2"}) == ~s({"foo":"foo1","foo":"foo2"})

    assert to_json(%{42 => :bar}) == ~s({"42":"bar"})
    assert to_json(%{42 => :bar}, bigint_as_string: true) == ~s({"42":"bar"})
    assert to_json(%{42.5 => :bar}) == ~s({"42.5":"bar"})
    assert to_json(%{42.5 => :bar}, bigint_as_string: true) == ~s({"42.5":"bar"})
    assert to_json(%{(2 ** 64) => :bar}) == ~s({"18446744073709551616":"bar"})

    assert to_json(%{(2 ** 64) => :bar}, bigint_as_string: true) ==
             ~s({"18446744073709551616":"bar"})

    assert to_json(%{(2 ** 100) => :bar}) == ~s({"1267650600228229401496703205376":"bar"})

    assert to_json(%{(2 ** 100) => :bar}, bigint_as_string: true) ==
             ~s({"1267650600228229401496703205376":"bar"})
  end

  test "list" do
    assert to_json([]) == "[]"
    assert to_json([1, 2, 3]) == "[1,2,3]"
  end

  test "tuple" do
    assert to_json({}) == "[]"
    assert to_json({1, 2, 3}) == "[1,2,3]"
  end

  test "strip struct" do
    struct = %Struct{}

    expected =
      Map.keys(struct)
      |> Enum.reverse()
      |> Enum.map(fn k ->
        v = get_in(struct, [Access.key(k)])
        ~s("#{k}":"#{v}")
      end)
      |> Enum.join(",")

    expected = "{#{expected}}"
    assert to_json(%Struct{}) == expected

    assert to_json(%Struct{}, strip_elixir_struct: true) == ~s({"field":"hi"})
  end

  defp to_json(value, opts \\ []) do
    {:ok, binary} = Jsont.encode(value, opts)
    binary
  end
end
