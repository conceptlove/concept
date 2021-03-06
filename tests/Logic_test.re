open TestHelper;
open Logic;

let x = var("x");
let y = var("y");
let z = var("z");

let expectAll = (vars: list(value), input: list(clause)) =>
  expect(run(vars, all(input))->Series.final);

describe("walk", () => {
  let state =
    [(name(x), y), (name(y), String("found!"))]->State.fromList;

  test("walk value", () =>
    expect(walk(String("return me"), state))
    |> toEqual(String("return me"))
  );

  test("walk direct", () =>
    expect(walk(y, state)) |> toEqual(String("found!"))
  );

  test("walk indirect", () =>
    expect(walk(x, state)) |> toEqual(String("found!"))
  );

  test("walk missing", () =>
    expect(walk(z, state)) |> toEqual(z)
  );
});

describe("deepwalk", () => {
  let state =
    [(name(x), y), (name(y), List([String("found!")]))]->State.fromList;

  test("direct", () =>
    expect(deepwalk(y, state)) |> toEqual(List([String("found!")]))
  );

  test("indirect", () =>
    expect(deepwalk(x, state)) |> toEqual(List([String("found!")]))
  );
});

// describe("unify", () => {
//   test("eq", () =>
//     expect(eq(x, Int(3)) |> unify()) |> toEqual([(x, Int(3))])
//   )
// });

describe("run", () => {
  test("eq", () =>
    expect(eq(x, Int(3)) |> run([x]) |> Series.final)
    |> toEqual([(x, Int(3))])
  );

  test("=:", () =>
    expect(x =: Int(3) |> run([x]) |> Series.final)
    |> toEqual([(x, Int(3))])
  );

  test("expectAll", () =>
    [x =: Int(3)] |> expectAll([x]) |> toEqual([(x, Int(3))])
  );

  // expect to raise
  // test("missing", () =>
  //   [] |> expectAll |> toEqual([(x, Int(3))])
  // );

  test("all indirect", () =>
    [x =: y, y =: Int(3), z =: z]
    |> expectAll([x])
    |> toEqual([(x, Int(3))])
  );
});

describe("patterns and analogy", () => {
  test("<->", () =>
    [id("FPGA") <-> id("Brain")] |> expectAll([x]) |> toEqual([(x, x)])
  );

  let m = id;
  let c = id;

  test("<->", () =>
    [
      c("Money") <-> m("Mana"),
      c("Purchase") <-> m("Manifestation"),
      c("Wealth") <-> m("Money"),
    ]
    |> expectAll([x])
    |> toEqual([(x, x)])
  );
});
