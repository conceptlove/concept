open Parser;

type expr =
  | List(list(expr))
  | Ident(string)
  | String(string)
  | Int(int)
  | Float(float);

type statement =
  | Eq(expr, expr);

let int = int->map(x => Int(x));
let float = Parser.float->map(x => x->Float);

let literal = oneOf([int, float]);

let ident = letters->map(x => x->Ident);
let expr = oneOf([literal, ident]);

// let eq = succeed((a, b) => Eq(a, b)) &= expr &. char('=')->spaced &= expr;
let eq =
  succeed((a, b) => Eq(a, b))
  &= expr
  &. space->many
  &. char('=')
  &. space->many
  &= expr;

let statement = oneOf([eq]);
