open Jest;
open Expect;
open Db;

let expected = (expected: state, input: string) =>
  expect(input |> parse) |> toEqual(expected);

describe("parsing", () => {
  test("pushing", () =>
    {|
push "abc"
push "def"
|} |> expected({stack: ["abc", "def"]})
  )
});
