type name = string;

module SMap = {
  include Map.Make({
    type t = string;
    let compare = compare;
  });
};

type value =
  | Dot
  | Map(SMap.t(value))
  | List(list(value))
  | Var(name)
  | String(string)
  | Int(int)
  | Float(float);

exception Not_a_var;

let name = value =>
  switch (value) {
  | Var(name) => name
  | _ => raise(Not_a_var)
  };

module State = {
  include SMap;

  module Op = {
    let (-->) = (a, b) => (a, b);
  };

  let get = (key: value) => find(name(key));

  let fromList = (xs: list((string, 'a))) =>
    xs |> List.fold_left((st, (k, v)) => add(k, v, st), empty);
};

type sMap = State.t(value);

let varCounter = ref(1);
let var = (name: string) =>
  ["~var", string_of_int(varCounter^), name]
  |> String.concat(":")
  |> (a => Var(a));

// Like a var, but singletons.
let id = (name: string) => Var("~id:" ++ name);

let rec walk = (key: value, sMap: sMap) => {
  switch (key) {
  | Var(k) =>
    switch (sMap |> State.find(k)) {
    | value => walk(value, sMap)
    | exception Not_found => key
    }
  | _ => key
  };
};

let rec deepwalk = (key: value, sMap: sMap): value =>
  switch (walk(key, sMap)) {
  | List(xs) => List(deepwalkList(sMap, xs))
  | x => x
  }
and deepwalkList = (sMap: sMap, xs: list(value)): list(value) =>
  switch (xs) {
  | [] => []
  // TODO: figure out this dot logic
  // | [Dot, x, ..._] => [deepwalk(x, sMap)]
  | [x, ...rest] => [x, ...deepwalkList(sMap, rest)]
  };

exception Unify_failed;

let rec unify = (x: value, y: value, sMap: sMap): sMap => {
  let x = walk(x, sMap);
  let y = walk(y, sMap);

  switch (x, y) {
  | (x, y) when x === y => sMap

  | (Var(id), v)
  | (v, Var(id)) => sMap |> State.add(id, v)

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

type clause = sMap => Series.t(sMap);

let run = (vars: list(value), goal: clause) => {
  let sMap = State.empty;
  goal(sMap)
  ->Series.map(sMap => {
      vars |> List.map(var => (var, deepwalk(var, sMap)))
    });
};

let eq = (x, y, sMap) => Series.from(unify(x, y, sMap));

let and_ = (clauses: Series.t(clause), sMap): Series.t(sMap) => {
  clauses->Series.flatScan((sMap, clause) => clause(sMap), sMap);
};

let all = clauses => clauses->Series.fromList->and_;

let conso = (first, rest, out) =>
  switch (rest) {
  | Var(_) => eq(List([first, Dot, rest]), out)
  | List(xs) => eq(List([first, ...xs]), out)
  | _ => raise(Unify_failed)
  };

let cons = (first, rest) =>
  switch (rest) {
  | Var(_) => List([first, Dot, rest])
  | List(xs) => List([first, ...xs])
  | _ => raise(Unify_failed)
  };

let emptyo = x => eq(x, List([]));
let empty = List([]);

let firsto = (first, out, ()) => {
  let rest = var("rest");
  conso(first, rest, out);
};

let first = (first, ()) => {
  cons(first, var("rest"));
};

let resto = (rest, out, ()) => {
  conso(var("first"), rest, out);
};

let rest = (rest, ()) => {
  cons(var("first"), rest);
};

let (=:) = eq;
let (<->) = eq;
