type input =
  | Slice(string, int);

type step('a) =
  | Good('a, input)
  | Bad(string);

type t('a) =
  | Parser(input => step('a));

exception Parse_error(string);

let slice = str => Slice(str, 0);
let length = (Slice(str, offset)) => str->String.length - offset;
let first = (Slice(str, n)) => str.[n];
let next = (Slice(str, n)) => Slice(str, n + 1);
let substr = (Slice(str, offset), n) => String.sub(str, offset, n);

let reduce = (fn, list) =>
  switch (list) {
  | [] => raise(Not_found)
  | [x, ...xs] => List.fold_left(fn, x, xs)
  };

let parse = (Parser(fn), input) => fn(input);

let run = (Parser(fn), str) =>
  switch (str->slice->fn) {
  | Good(a, _) => a
  | Bad(msg) => raise(Parse_error(msg))
  };

let succeed = x => Parser(input => Good(x, input));
let failure = msg => Parser(_ => Bad(msg));

let map = (parser, f) =>
  Parser(
    input =>
      switch (parse(parser, input)) {
      | Good(value, remaining) => Good(f(value), remaining)
      | Bad(err) => Bad(err)
      },
  );

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

let orElse = (a: t('a), b: t('a)): t('a) =>
  Parser(
    input =>
      switch (a->parse(input)) {
      | Good(x, r) => Good(x, r)
      | Bad(_err) => b->parse(input)
      },
  );

let andTry = (a: t('a), fn: 'a => t('a)): t('a) =>
  a->andThen(v => fn(v)->orElse(succeed(v)));

let withDefault = (parser, value) => parser->orElse(succeed(value));

let oneOf = reduce(orElse, _);

let charIf = (fn: char => bool) =>
  Parser(
    input =>
      if (input->length <= 0) {
        Bad("Unexpected end of input");
      } else if (fn(first(input))) {
        Good(input->substr(1), next(input));
      } else {
        let msg = Printf.sprintf("Unexpected '%c'.", first(input));
        Bad(msg);
      },
  );

let range = (a, b) => charIf(ch => ch >= min(a, b) && ch <= max(a, b));

let tuple = (a, b) =>
  Parser(
    input =>
      switch (parse(a, input)) {
      | Bad(err) => Bad(err)
      | Good(value1, rest) =>
        switch (parse(b, rest)) {
        | Bad(err) => Bad(err)
        | Good(value2, rest) => Good((value1, value2), rest)
        }
      },
  );

let keep = (fP, xP) => fP->tuple(xP)->map(((f, x)) => f(x));
let skip = (fP, xP) => fP->tuple(xP)->map(((f, _)) => f);

let rec fold = (parser, init, fn) =>
  parser
  ->andThen(x => fold(parser, fn(init, x), fn))
  ->orElse(succeed(init));

let flag = (parser, fn) => parser->map(_ => fn)->withDefault(x => x);

let append = (a, b) => succeed((++))->keep(a)->keep(b);

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

let char = c => charIf(c2 => c === c2);
let lower = range('a', 'z');
let upper = range('A', 'Z');
let letter = lower->orElse(upper);

let digit = range('0', '9');
let digits = digit->fold("", (++));

let posInt = digits->map(int_of_string);
let int = char('-')->flag(( * )(-1))->keep(posInt);

let posFloat =
  digits->append(char('.'))->append(digits)->map(float_of_string);
let float = char('-')->flag(( *. )(-1.))->keep(posFloat);
