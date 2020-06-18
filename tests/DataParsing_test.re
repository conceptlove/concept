open TestHelper;
open DataParsing;
open Data;

let run = Parser.run;

describe("statement", () => {
  test("eq", () =>
    expect(eq->run("123 = x")) |> toEqual(Eq(Int(123), Ident("x")))
  )
});
