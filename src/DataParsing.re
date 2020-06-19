open Parser;
open DataAst;

let int = int->map(x => Int(x));
let float = Parser.float->map(x => x->Float);
let string = string->map(x => String(x));

let literal = oneOf([int, float, string]);

let ident = letters->map(x => x->Ident);

let expr = {
  let rec exp = () => oneOf([literal, ident, parens(exp->lazy_)]);
  exp();
};

let eq =
  succeed((a, b) => Eq(a, b))
  &= expr
  &. spaces
  &. char('=')
  &. spaces
  &= expr;

let statement = oneOf([eq]);
