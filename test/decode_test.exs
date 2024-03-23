defmodule Jsont.DecodeTest do
  use Snapshy
  use ExUnit.Case, async: true
  use ExUnitProperties

  test "numbers" do
    assert_fail("-")
    assert_fail("--1")
    assert_fail("01")
    assert_fail(".1")
    assert_fail("1.")
    assert_fail("1e")
    assert_fail("1.0e+")
    assert_fail("1e999")

    assert parse!("0") == 0
    assert parse!("1") == 1
    assert parse!("-0") == 0
    assert parse!("-1") == -1
    assert parse!("0.1") == 0.1
    assert parse!("-0.1") == -0.1
    assert parse!("0e0") == 0
    assert parse!("0E0") == 0
    assert parse!("1e0") == 1
    assert parse!("1E0") == 1
    assert parse!("1.0e0") == 1.0
    assert parse!("1e+0") == 1
    assert parse!("1.0e+0") == 1.0
    assert parse!("0.1e1") == 0.1e1
    assert parse!("0.1e-1") == 0.1e-1
    assert parse!("99.99e99") == 99.99e99
    assert parse!("-99.99e-99") == -99.99e-99
    assert parse!("123456789.123456789e123") == 123_456_789.123456789e123

    assert parse!("-9223372036854775808") == -9_223_372_036_854_775_808
    assert parse!("-9223372036854775809") == -9_223_372_036_854_775_809
    assert parse!("9223372036854775807") == 9_223_372_036_854_775_807
    assert parse!("9223372036854775808") == 9_223_372_036_854_775_808
  end

  test "number" do
    check all(int <- integer()) do
      assert parse!(Integer.to_string(int)) == int
    end

    check all(value <- float()) do
      assert parse!(Float.to_string(value)) == value
    end
  end

  test "strings" do
    assert_fail(~s("))
    assert_fail(~s("\\"))
    assert_fail(~s("\\k"))
    assert_fail(~s("a\r"))
    assert_fail(~s("\\u2603\\"))
    assert_fail(~s("Here's a snowman for you: ☃. Good day!))
    assert_fail(~s("𝄞))
    assert_fail(~s(\u001F))
    assert_fail(~s("\\ud8aa\\udcxx"))
    assert_fail(~s("\\ud8aa\\uda00"))
    assert_fail(~s("\\uxxxx"))

    assert_fail(<<?", 128, ?">>, validate_unicode: true)
    assert parse!(<<?", 128, ?">>, validate_unicode: false) == <<128>>

    assert parse!(~s("\\"\\\\\\/\\b\\f\\n\\r\\t")) == ~s("\\/\b\f\n\r\t)
    assert parse!(~s("\\u2603")) == "☃"
    assert parse!(~s("\\u2028\\u2029")) == "\u2028\u2029"
    assert parse!(~s("\\uD834\\uDD1E")) == "𝄞"
    assert parse!(~s("\\uD834\\uDD1E")) == "𝄞"
    assert parse!(~s("\\uD799\\uD799")) == "힙힙"
    assert parse!(~s("✔︎")) == "✔︎"
  end

  test "objects" do
    assert_fail("{")
    assert_fail("{,")
    assert_fail(~s({"foo"}))
    assert_fail(~s({"foo",}))
    assert_fail(~s({"foo": "bar",}))
    assert_fail(~s({"foo": "bar":}))
    assert_fail("{{}}")
    assert_fail("}")

    assert parse!("{}") == %{}
    assert parse!(~s({"foo": "bar"})) == %{"foo" => "bar"}
    assert parse!(~s({"foo"  : "bar"})) == %{"foo" => "bar"}

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert parse!(~s({"foo": "bar", "baz": "quux"})) == expected

    expected = %{"foo" => %{"bar" => "baz"}}
    assert parse!(~s({"foo": {"bar": "baz"}})) == expected

    assert parse!(~s({"a": 1, "a": 2})) == %{"a" => 2}
  end

  test "arrays" do
    assert_fail("[")
    assert_fail("[,")
    assert_fail("[1,]")

    assert parse!("[]") == []
    assert parse!("[1, 2, 3]") == [1, 2, 3]
    assert parse!(~s(["foo", "bar", "baz"])) == ["foo", "bar", "baz"]
    assert parse!(~s([{"foo": "bar"}])) == [%{"foo" => "bar"}]
  end

  test "whitespace" do
    assert_fail("")
    assert_fail("    ")

    assert parse!("  [  ]  ") == []
    assert parse!("  {  }  ") == %{}

    assert parse!("  [  1  ,  2  ,  3  ]  ") == [1, 2, 3]

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert parse!(~s(  {  "foo"  :  "bar"  ,  "baz"  :  "quux"  }  )) == expected
  end

  test "large integers" do
    binary = String.duplicate("1", 2_000)
    assert parse!(binary) == :erlang.binary_to_integer(binary)
  end

  test "iodata" do
    body = String.split(~s([1,2,3,4]), "")
    expected = [1, 2, 3, 4]
    assert parse!(body) == expected
  end

  describe "Bench Fixtures" do
    root = Path.expand(Path.join(Path.dirname(__ENV__.file), "/../bench/data"))

    for path <- Path.wildcard("#{root}/*") do
      test "bench input #{path}" do
        {:ok, contents} = File.read(unquote(path))
        parse!(contents)
      end
    end
  end

  describe "JSONTestSuite" do
    root = Path.expand(Path.join(__DIR__, "../JSONTestSuite/test_parsing"))

    for path <- Path.wildcard("#{root}/y_*.json") do
      file = Path.basename(path, ".json")

      test_snapshot "#{file} passes" do
        data = File.read!(unquote(path))
        {data, parse!(data)}
      end
    end

    for path <- Path.wildcard("#{root}/n_*.json") do
      file = Path.basename(path, ".json")

      test "#{file} fails" do
        data = File.read!(unquote(path))
        assert_raise RuntimeError, fn -> parse!(data) end
      end
    end
  end

  defp parse!(json, opts \\ []) do
    case Jsont.decode(json, opts) do
      {:ok, term} ->
        term

      {:error, error} ->
        raise inspect(error, pretty: true)
    end
  end

  defp assert_fail(string, opts \\ []) do
    {:error, _} = Jsont.decode(string, opts)
  end
end
