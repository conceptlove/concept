type t('a) =
  // | Loading(ms, unit => t('a)) // No value yet, call again in the future.
  | Step('a, unit => t('a))
  | End;

// Should be possible to contain a whole list of computed values, rather than only the most recent one.

let empty = End;

let from = (x: 'a) => Step(x, () => End);

let push = (st: t('a), x: 'a): t('a) => Step(x, () => st);

let rec fromList = (xs: list('a)): t('a) =>
  switch (xs) {
  | [] => End
  | [x, ...rest] => Step(x, () => fromList(rest))
  };

let rec range = (a: int, b: int) =>
  if (a === b) {
    Step(a, () => End);
  } else {
    Step(a, () => range(a + 1, b));
  };

let rec toList = (st: t('a)): list('a) =>
  switch (st) {
  | End => []
  | Step(x, nxt) => [x, ...toList(nxt())]
  };

let peek = (st: t('a)): 'a =>
  switch (st) {
  | Step(x, _) => x
  | End => raise(Not_found)
  };

let head = peek;

let tail = (st: t('a)): t('a) =>
  switch (st) {
  | Step(_, nxt) => nxt()
  | End => End
  };

// TODO: not sure this is correct
let rec scan = (st: t('a), fn: ('b, 'a) => 'b, init: 'b): t('b) =>
  switch (st) {
  | Step(x, nxt) =>
    let b = fn(init, x);
    Step(b, () => scan(nxt(), fn, b));
  | End => Step(init, () => End)
  };

// TODO: not sure this is correct
let accum = scan(_, (xs, x) => [x, ...xs], []);

let rec limit = (st: t('a), n: int): t('a) =>
  switch (n) {
  | 0 => End
  | _ =>
    switch (st) {
    | End => End
    | Step(x, nxt) => Step(x, () => nxt()->limit(n - 1))
    }
  };

let rec drop = (st: t('a), n: int): t('a) =>
  switch (n) {
  | 0 => st
  | _ => st->tail->drop(n - 1)
  };

let rec map = (st: t('a), fn: 'a => 'b): t('b) => {
  switch (st) {
  | Step(x, nxt) => Step(fn(x), () => nxt()->map(fn))
  | End => End
  };
};

let rec flatten = (st: t(t('a))): t('a) => {
  switch (st) {
  | Step(Step(x, nyt), nxt) => Step(x, () => Step(nyt(), nxt)->flatten)
  | Step(End, nxt) => nxt()->flatten
  | End => End
  };
};

let flatMap = (st: t('a), fn: 'a => t('b)): t('b) => {
  st->map(fn)->flatten;
};
