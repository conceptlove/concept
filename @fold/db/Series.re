type t('a) =
  // | Loading(ms, unit => t('a)) // No value yet, call again in the future.
  | Step('a, unit => t('a))
  | End;

let empty = () => End;

let rec nop = Step((), () => nop);

let from = (x: 'a) => Step(x, () => End);

let push = (st: t('a), x: 'a): t('a) => Step(x, () => st);

let rec append = (before: t('a), after: t('a)): t('a) =>
  switch (before) {
  | Step(a, nxt) => Step(a, () => append(nxt(), after))
  | End => after
  };

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

let rec final = (st: t('a)): 'a =>
  switch (st) {
  | End => raise(Not_found)
  | Step(a, nxt) =>
    switch (nxt()) {
    | End => a
    | st => final(st)
    }
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

let take = limit;

let pull = (st: t('a), n: int): list('a) => st->take(n)->toList;

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

let flatScan = (st: t('a), fn: ('b, 'a) => t('b), init: 'b): t('b) =>
  st->scan((bSt, a) => bSt->flatMap(b => fn(b, a)), from(init))->flatten;

// let rec delta = (st: t('a), init: 'a, fn: ('a, 'a) => 'b): t('b) =>
//   switch (st) {
//   | End => End
//   | Step(n, nxt) => Step(fn(init, n), () => delta(nxt()))
//   };

let rec step = (init: 'a, nxt: 'a => 'a): t('a) =>
  Step(init, () => step(nxt(init), nxt));

let rec gen = (fn: ('a, 'a) => 'a, a: 'a, b: 'a) =>
  Step(a, () => gen(fn, b, fn(a, b)));

let genAdd = gen((a, b) => a + b);

let fib = genAdd(0, 1);
