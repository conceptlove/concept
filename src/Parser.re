type input =
  | Slice(string, int, int);

type step('a) =
  | Good('a, string)
  | Bad(string);

type t('a) =
  | Parser(string => step('a));

exception Parse_error(string);

let reduce = (fn, list) =>
  switch (list) {
  | [] => raise(Not_found)
  | [x, ...xs] => List.fold_left(fn, x, xs)
  };

let parse = (Parser(fn), input) => fn(input);

let run = (Parser(fn), input) =>
  switch (fn(input)) {
  | Good(a, _) => a
  | Bad(msg) => raise(Parse_error(msg))
  };

let succeed = x => Parser(input => Good(x, input));
let failure = Parser(input => Bad(input));

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

let tuple = (a, b) =>
  Parser(
    input =>
      switch (parse(a, input)) {
      | Bad(err) => Bad(err)
      | Good(value1, remaining1) =>
        switch (parse(b, remaining1)) {
        | Bad(err) => Bad(err)
        | Good(value2, remaining2) => Good((value1, value2), remaining2)
        }
      },
  );

let keep = (fP, xP) => fP->tuple(xP)->map(((f, x)) => f(x));
let skip = (fP, xP) => fP->tuple(xP)->map(((f, _)) => f);

let orElse = (a: t('a), b: t('a)): t('a) =>
  Parser(
    input =>
      switch (a->parse(input)) {
      | Good(x, r) => Good(x, r)
      | Bad(_err) => b->parse(input)
      },
  );

let oneOf = reduce(orElse, _);

let range = (a, b) =>
  Parser(
    str =>
      if (str->String.length == 0) {
        Bad("Unexpected end of input");
      } else if (str.[0] < min(a, b) || str.[0] > max(a, b)) {
        let msg =
          Printf.sprintf("Expected '%c' to '%c'. Got '%c'", a, b, str.[0]);
        Bad(msg);
      } else {
        Good(str->String.sub(0, 1), str->(Js.String.sliceToEnd(~from=1)));
      },
  );

let rec oneOrMore = parser => parser->andThen(_ => oneOrMore(parser));

// let many = parser =>
//   parser->andThen(result =>
//     Parser(input =>

//     )
//   )

// let applyP = (fP, xP) => xP->andThen(fP)->map(((f, x)) => f(x));
// let (|.) = (Parser(keep), Parser(ignor)) => Parser(keep);
// let (|=) = (Parser(fn), Parser(v)) => Parser(str => fn(v(str)));

let char = c => range(c, c);
let lower = range('a', 'z');
let upper = range('A', 'Z');
let letter = lower->orElse(upper);

let digit = range('0', '9');

// let int = many(digit) |> map(int_of_string);
let float = succeed(0);
