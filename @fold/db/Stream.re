type t('a) =
  // | Loading(ms, unit => t('a)) // No value yet, call again in the future.
  | Step('a, unit => t('a))
  | End;

let from = (x: 'a) => Step(x, () => End);

let push = (st: t('a), x: 'a): t('a) => Step(x, () => st);

let rec fromList = (xs: list('a)): t('a) =>
  switch (xs) {
  | [] => End
  | [x, ...rest] => rest->fromList->push(x)
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

let retail = (st: t('a), fn: t('a) => t('a)): t('a) => st;

let rec limit = (st: t('a), n: int): t('a) =>
  switch (n) {
  | 0 => End
  | _ => st->tail->limit(n - 1)
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
  | Step(End, nxt) => nxt()->flatten
  | Step(Step(x, nyt), nxt) => Step(x, () => Step(nyt(), nxt)->flatten)
  | End => End
  };
};

let flatMap = (st: t('a), fn: 'a => t('b)): t('b) => {
  st->map(fn)->flatten;
};
