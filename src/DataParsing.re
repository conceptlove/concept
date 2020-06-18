open Parser;
open Data;

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
