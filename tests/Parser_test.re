open TestHelper;
open Parser;

let toFail = string => toEqual(Bad(string));

let expectParser = (parser, string) => expect(parser->run(string));

describe("Input", () => {
  open Input;

  test("slice", () =>
    expect(slice("abcdef")) |> toEqual(Slice("abcdef", 0))
  );

  let a = Slice("abcdef", 3);

  test("length", () =>
    expect(a->length) |> toEqual(3)
  );

  test("first", () =>
    expect(a->first) |> toEqual('d')
  );

  test("next", () =>
    expect(a->next) |> toEqual(Slice("abcdef", 4))
  );

  test("substr", () =>
    expect(a->substr(3)) |> toEqual("def")
  );
});

describe("Parser", () => {
  let a = char('A');
  let b = char('B');

  test("range", () =>
    expectParser(range('a', 'b'), "abc") |> toEqual("a")
  );

  test("lower", () =>
    expectParser(lower, "def") |> toEqual("d")
  );

  test("upper", () =>
    expectParser(upper, "DEF") |> toEqual("D")
  );

  test("digit", () =>
    expectParser(digit, "123") |> toEqual("1")
  );

  test("a map2 b", () =>
    expectParser(a->map2(b, (++)), "ABC") |> toEqual("AB")
  );

  test("a orElse b", () =>
    expectParser(a->orElse(b), "AFB") |> toEqual("A")
  );

  test("a andTry good b", () =>
    expectParser(a->andTry(_ => b), "ABC") |> toEqual("B")
  );

  test("a andTry bad b", () =>
    expectParser(a->andTry(_ => b), "AFG") |> toEqual("A")
  );

  test("a andThen b", () => {
    let charInc = ch => Char.chr(ch->Char.code + 1);
    expectParser(letter->andThen(str => char(str.[0]->charInc)), "LMN")
    |> toEqual("M");
  });

  let spaces = char(' '); //->oneOrMore()
  let tup2 = (a, b) => (a, b);

  test("keep and skip", () =>
    expectParser(
      succeed(tup2)->keep(letter)->skip(spaces)->keep(letter),
      "a b",
    )
    |> toEqual(("a", "b"))
  );

  test("append", () =>
    expectParser(letter->append(letter), "abc") |> toEqual("ab")
  );

  test("repeat", () =>
    expect(a->repeat("x", (++))->run("A")) |> toEqual("xA")
  );

  test("repeat 4", () =>
    expect(a->repeat("x", (++))->run("AAAA")) |> toEqual("xAAAA")
  );

  test("repeat 4 with bad ending", () =>
    expect(a->repeat("x", (++))->run("AAAAbad")) |> toEqual("xAAAA")
  );

  test("many", () =>
    expectParser(char('a')->many, "aaaB") |> toEqual("aaa")
  );

  test("digits", () =>
    expectParser(digits, "123") |> toEqual("123")
  );

  test("digits and extra", () =>
    expectParser(digits, "123ab") |> toEqual("123")
  );

  test("int", () =>
    expectParser(int, "123ab") |> toEqual(123)
  );

  test("negative int", () =>
    expectParser(int, "-123ab") |> toEqual(-123)
  );

  test("float", () =>
    expectParser(Parser.float, "123.45") |> toEqual(123.45)
  );

  test("negative float", () =>
    expectParser(Parser.float, "-123.45") |> toEqual(-123.45)
  );

  test("underscores", () =>
    expect(char('_')->many->run("___x")) |> toEqual("___")
  );

  test("space", () =>
    expect(space->run("   x")) |> toEqual(" ")
  );

  test("many space", () =>
    expect(space->many->run("   x")) |> toEqual("   ")
  );

  let spaces2 = space->many;

  test("spaces2", () =>
    expect(spaces2->run("   x")) |> toEqual("   ")
  );

  test("spaces2 2", () =>
    expect(spaces2->run("   x")) |> toEqual("   ")
  );

  test("spaced x", () =>
    expect(char('x')->spaced->run("  x  ")) |> toEqual("x")
  );

  test("simple string", () =>
    expect(string->run({|"testing"|})) |> toEqual("testing")
  );

  test("string with newline", () =>
    expect(string->run({|"\ntesting\n"|})) |> toEqual("\ntesting\n")
  );

  test("string with escaped newline", () =>
    expect(string->run({|"hi \
there"|})) |> toEqual("hi there")
  );

  test("string with escaped quote", () =>
    expect(string->run({|"\"testing\""|})) |> toEqual({|"testing"|})
  );
});
