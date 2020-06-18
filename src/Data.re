type expr =
  | List(list(expr))
  | Ident(string)
  | String(string)
  | Int(int)
  | Float(float);

type statement =
  | Eq(expr, expr);
