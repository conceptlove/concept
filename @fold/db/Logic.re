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
  | List(xs) => List(deepwalkList(sMap, xs))
  | value => value
  };
}
and deepwalkList = (sMap: sMap, xs: list(value)): list(value) =>
  switch (xs) {
  | [] => []
  // TODO: these need work
  | [Dot, x, ...xs] => [deepwalk(x, sMap)]
  | [x, ...xs] => List.append(deepwalkList(sMap, xs), [deepwalk(x, sMap)])
  };

exception Unify_failed;

let rec unify = (x, y, sMap) => {
  let x = walk(x, sMap);
  let y = walk(y, sMap);

  switch (x, y) {
  | (x, y) when x === y => sMap
  | (Var(id), y) => sMap |> ImmutableMap.add(id, y)
  | (x, Var(id)) => sMap |> ImmutableMap.add(id, x)
  | (List(xs), List(ys)) => unifyList(xs, ys, sMap)
  | _ => raise(Unify_failed)
  };
}
and unifyList = (xs: list(value), ys: list(value), sMap: sMap) => {
  switch (xs, ys) {
  | ([], []) => sMap

  | ([Dot, x, ..._], ys)
  | (ys, [Dot, x, ..._]) => unify(x, List(ys), sMap)

  | ([], [_, ..._])
  | ([_, ..._], []) => raise(Unify_failed)

  | ([x, ...xs], [y, ...ys]) => unifyList(xs, ys, unify(x, y, sMap))
  };
};

type clause = sMap => Stream.t(sMap);

let eq = (x, y, sMap) => Stream.from(unify(x, y, sMap));

let and_ = (clauses: Stream.t(clause), sMap): Stream.t(sMap) => {
  clauses->Stream.flatMap(clause => clause(sMap));
};

let run = (vars: list(string), goal: clause): Stream.t(sMap) => {};
