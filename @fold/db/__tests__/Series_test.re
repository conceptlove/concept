open Jest;
open Expect;
open Series;

// let expected = (expected: (value, value), input: clause) =>
//   expect(run([x], input)->limit(1)->toList->List.flatten)
//   |> toEqual([expected]);

describe("lists", () => {
  test("fromList toList", () => {
    expect([1, 2, 3]->fromList->toList) |> toEqual([1, 2, 3])
  })
});

describe("getters", () => {
  let st = range(1, 3);

  test("empty", () =>
    expect(empty()) |> toEqual(End)
  );

  test("empty toList", () =>
    expect(empty()->toList) |> toEqual([])
  );

  test("toList", () =>
    expect(st->toList) |> toEqual([1, 2, 3])
  );

  test("peek", () =>
    expect(st->peek) |> toEqual(1)
  );

  test("tail", () =>
    expect(st->tail->toList) |> toEqual([2, 3])
  );

  test("tail x 3", () =>
    expect(st->tail->tail->tail) |> toEqual(End)
  );

  test("drop 0", () =>
    expect(st->drop(0)->toList) |> toEqual([1, 2, 3])
  );

  test("drop 1", () =>
    expect(st->drop(1)->toList) |> toEqual([2, 3])
  );

  test("drop 3", () =>
    expect(st->drop(3)->toList) |> toEqual([])
  );

  test("limit 0", () =>
    expect(st->limit(0)) |> toEqual(End)
  );

  test("limit 1", () =>
    expect(st->limit(1)->toList) |> toEqual([1])
  );

  test("limit 3", () =>
    expect(st->limit(3)->toList) |> toEqual([1, 2, 3])
  );

  test("append", () =>
    expect(st->append(fromList([4, 5, 6]))->toList)
    |> toEqual([1, 2, 3, 4, 5, 6])
  );
});

describe("gen", () => {
  test("fib", () =>
    expect(fib->take(8)->toList) |> toEqual([0, 1, 1, 2, 3, 5, 8, 13])
  )
});
