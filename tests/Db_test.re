open TestHelper;
open Db;

let expected = (expected: state, input: list(op)) =>
  expect(input |> stepAll(empty)) |> toEqual(expected);

let expectSteps = (ops: list(op)) => expect(ops |> stepAll(empty));

let toStack = (stack: list(string)) => toEqual({facts: [], stack});

describe("stepping", () => {
  test("push", () =>
    expectSteps([Push("ent-id"), Push("attr-id")])
    |> toStack(["attr-id", "ent-id"])
  );

  test("pop empty", () =>
    expectSteps([Pop]) |> toStack([])
  );

  test("pop", () =>
    expectSteps([Push("ent-id"), Push("attr-id"), Pop])
    |> toStack(["ent-id"])
  );

  test("pop at end", () =>
    expectSteps([Push("ent-id"), Pop, Pop]) |> toStack([])
  );
});
