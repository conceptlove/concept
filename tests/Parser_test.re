open TestHelper;
open Parser;

let toFail = string => toEqual(Bad(string));

let expectParser = (parser, string) => expect(parser->run(string));

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

  test("a tuple b", () =>
    expectParser(a->tuple(b), "ABC") |> toEqual(("A", "B"))
  );

  test("a orElse b", () =>
    expectParser(a->orElse(b), "AFB") |> toEqual("A")
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

  test("digits", () =>
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
});
