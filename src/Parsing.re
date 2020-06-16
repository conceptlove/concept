open Parser;

type literal =
  | String(string)
  | Int(int)
  | Float(float);

type expr =
  | List(list(expr))
  | Var(string)
  | Literal(literal);

type statement =
  | Eq(expr, expr);

let int = Parser.int->map(x => Int(x));
let float = Parser.float->map(x => Float(x));

let literal = oneOf([int, float]);

let expr = oneOf([literal->map(x => Literal(x))]);

let eq = succeed((a, b) => Eq(a, b)) &= expr &. char('=') &= expr;
