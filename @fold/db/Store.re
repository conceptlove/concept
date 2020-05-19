module Children =
  Map.Make({
    type t = string;
    let compare = compare;
  });

type node('a) =
  | Leaf('a)
  | Branch('a, Children.t(node('a)))
  | Root(Children.t(node('a)));

let empty = Root(Children.empty);
