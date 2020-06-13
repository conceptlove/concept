type value =
  | String(string);

type fact =
  | Set(list(string), value);

type op =
  | Push(string)
  | Pop;

type state = {
  stack: list(string),
  facts: list(string),
};

let empty = {stack: [], facts: []};

let step = (state: state, op: op) =>
  switch (op, state) {
  | (Push(term), _) => {...state, stack: [term, ...state.stack]}

  | (Pop, {stack: [_, ...stack]}) => {...state, stack}
  | (Pop, {stack: []}) => state
  };

let stepAll = List.fold_left(step);
