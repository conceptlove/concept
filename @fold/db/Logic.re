type name = string;

type value =
  | Dot
  | List(list(value))
  | Var(name)
  | String(string)
  | Int(int)
  | Float(float);

module ImmutableMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

type sMap = ImmutableMap.t(value);

let (:=) = (x: string, y: string) => x ++ y;

let lvarCounter = ref(0);
let var = () => Var("~var:" ++ string_of_int(lvarCounter^));

let dot = () => ();

let rec walk = (key: value, sMap: sMap) => {
  switch (key) {
  | Var(k) =>
    let value =
      try(sMap |> ImmutableMap.find(k)) {
      | Not_found => key
      };

    walk(value, sMap);
  | _ => key
  };
};

let rec deepwalk = (key, sMap) => {
  switch (walk(key, sMap)) {
  | List(xs) => List(continue(sMap, xs))
  | value => value
  };
}
and continue = (sMap: sMap, xs: list(value)): list(value) =>
  switch (xs) {
  | [] => []
  // TODO: these need work
  | [Dot, x, ...xs] => [deepwalk(x, sMap)]
  | [x, ...xs] => List.append(continue(sMap, xs), [deepwalk(x, sMap)])
  };
