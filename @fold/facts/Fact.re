module T = {
  type str = string;
};

exception NotFound;

type id =
  | Id(string);

type value =
  | String(string)
  | Int(int)
  | Float(float);

let string = (_path: string, value: value) =>
  switch (value) {
  | String(str) => str
  | _ => raise(NotFound)
  };

let name = string("name");

type t =
  | Alias(string, string)
  | Insert(string, string);
