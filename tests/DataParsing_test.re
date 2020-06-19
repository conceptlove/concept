open TestHelper;
open DataParsing;
open DataAst;

let run = Parser.run;

describe("statement", () => {
  test("int literal", () =>
    expect(literal->run("123")) |> toEqual(Int(123))
  );

  test("eq", () =>
    expect(eq->run("123 = x")) |> toEqual(Eq(Int(123), Ident("x")))
  );
});
