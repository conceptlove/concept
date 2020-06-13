open Genlex;
open Stream;

type ast =
  | List(list(ast))
  | Var(string)
  | String(string)
  | Int(int)
  | Float(float);

let lexer = make_lexer(["push"]);

let parseLine = (input: string) => input |> of_string |> lexer;

// let buildOp = (tokens: list(token), stream: Stream.t(token)) =>
//   switch (tokens) {
//   | [Kwd("push"), String(x)] => Db.Push(x)
//   | _ =>
//   };

// let parse = (_input: string): list(Db.op) => {
//   [];
//     // input |> String.trim |> String.split_on_char('\n') |> List.map(parseLine);
// };
