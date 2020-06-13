open Jest;
open Expect;
open Parser;

let toParse = (step, remain) => toEqual(Good(step, remain));
let toFail = string => toEqual(Bad(string));

let expectParser = (parser, string) => expect(parser->parse(string));

describe("Parser", () => {
  let a = char('A');
  let b = char('B');

  test("range", () =>
    expectParser(range('a', 'b'), "abc") |> toParse("a", "bc")
  );

  test("lower", () =>
    expectParser(lower, "def") |> toParse("d", "ef")
  );

  test("upper", () =>
    expectParser(upper, "DEF") |> toParse("D", "EF")
  );

  test("digit", () =>
    expectParser(digit, "123") |> toParse("1", "23")
  );

  test("a then b", () =>
    expect(a->tuple(b)->parse("ABC")) |> toParse(("A", "B"), "C")
  );

  test("a orElse b", () =>
    expect(a->orElse(b)->parse("AFB")) |> toParse("A", "FB")
  );
});
