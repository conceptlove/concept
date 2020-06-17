open Core;

module Input = {
  type t =
    | Slice(string, int);

  let slice = str => Slice(str, 0);
  let length = (Slice(str, offset)) => str->String.length - offset;
  let first = (Slice(str, n)) => str.[n];
  let next = (Slice(str, n)) => Slice(str, n + 1);
  let substr = (Slice(str, offset), n) => String.sub(str, offset, n);
};

type input = Input.t;

type step('a) =
  | Good('a, input)
  | Bad(string);

type t('a) =
  | Parser(input => step('a));

exception Parse_error(string);

let trace = (tag, v) => {
  Js.log2(tag ++ ":", v);
  v;
};

let reduce = (fn, list) =>
  switch (list) {
  | [] => raise(Not_found)
  | [x, ...xs] => List.fold_left(fn, x, xs)
  };

let parse = (Parser(fn), input) => fn(input);

let run = (Parser(fn), str) =>
  switch (str->Input.slice->fn) {
  | Good(a, _) => a
  | Bad(msg) => raise(Parse_error(msg))
  };

let succeed = x => Parser(input => Good(x, input));
let failure = msg => Parser(_ => Bad(msg));
let start = () => succeed(x => x);

let map = (parser, f) =>
  Parser(
    input =>
      switch (parse(parser, input)) {
      | Good(value, remaining) => Good(f(value), remaining)
      | Bad(err) => Bad(err)
      },
  );

/** Pick a parser that depends on the value of another parser. */
let andThen = (Parser(parseA), fn: 'a => t('b)): t('b) =>
  Parser(
    input =>
      switch (parseA(input)) {
      | Bad(err) => Bad(err)
      | Good(a, restA) =>
        let Parser(parseB) = fn(a);

        switch (parseB(restA)) {
        | Bad(err) => Bad(err)
        | Good(b, restB) => Good(b, restB)
        };
      },
  );

let map2 = (a, b, fn) => a->andThen(v => b->map(fn(v)));

let orElse = (a: t('a), b: t('a)): t('a) =>
  Parser(
    input =>
      switch (a->parse(input)) {
      | Good(x, r) => Good(x, r)
      | Bad(_err) => b->parse(input)
      },
  );

/** Recover a Bad step with the given value. */
let withDefault = (parser, value) => parser->orElse(succeed(value));

let andTry = (a: t('a), fn: 'a => t('a)): t('a) =>
  a->andThen(v => fn(v)->withDefault(v));

let oneOf = reduce(orElse, _);

let charIf = (fn: char => bool) =>
  Parser(
    input =>
      if (input->Input.length <= 0) {
        Bad("Unexpected end of input");
      } else if (fn(Input.first(input))) {
        Good(input->Input.substr(1), Input.next(input));
      } else {
        let msg = Printf.sprintf("Unexpected '%c'.", Input.first(input));
        Bad(msg);
      },
  );

let range = (a, b) => charIf(ch => ch >= min(a, b) && ch <= max(a, b));

let keep = (f, x) => f->map2(x, apply);
let skip = (f, x) => f->map2(x, (f, _) => f);

let rec repeat = (parser, init: 'accum, fn) =>
  parser
  ->map(fn(init))
  ->andThen(v => parser->repeat(v, fn)->withDefault(v));

/** Quickly set a mapping function as the parser value. */
let flag = (parser, fn) => succeed(fn)->skip(parser)->withDefault(x => x);
let append = (a, b) => a->map2(b, (++));
let many = parser => parser->repeat("", (++));

// let rec charsWhile = (fn: char => bool): t(string) =>
//   charIf(fn)->andThen(a => charsWhile)

// let many = parser =>
//   parser->andThen(result =>
//     Parser(input =>

//     )
//   )

// let applyP = (fP, xP) => xP->andThen(fP)->map(((f, x)) => f(x));
let (&.) = skip;
let (&=) = keep;
// let (++=) = (a, b) => keep(append(b), a);

let char = c => range(c, c);
let lower = range('a', 'z');
let upper = range('A', 'Z');

let space = char(' ');
let spaces = space->many;

let spaced = parser => start()->skip(spaces)->keep(parser)->skip(spaces);

let letter = lower->orElse(upper);
let letters = letter->many;

let digit = range('0', '9');
let digits = digit->many;

let posInt = digits->map(int_of_string);
let int = char('-')->flag(x => - x)->keep(posInt);

let posFloat =
  succeed(float_of_string) &= digits->append(char('.'))->append(digits);

let float = char('-')->flag(x => -. x) &= posFloat;
