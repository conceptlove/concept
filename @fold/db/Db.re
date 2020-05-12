type value =
  | String(string);

type fact =
  | Set(list(string), value);

type op =
  | Push(string);

type state = {stack: list(string)};

let step = (op: op) =>
  switch (op) {
  | Push(term) => (
      (state: state) => {...state, stack: [term, ...state.stack]}
    )
  };

let lexer = Genlex.make_lexer(["push"]);

let parseLine = (input: string) =>
  input |> Stream.of_string |> lexer |> Stream.iter(token => {Js.log(token)});

let parse = (input: string) => {
  // |> String.split_on_char('\n')
  input |> String.trim |> String.split_on_char('\n') |> List.map(parseLine);
};
