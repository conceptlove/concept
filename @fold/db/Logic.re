type name = string;

type value =
  | Dot
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
  include Map.Make({
    type t = string;
    let compare = compare;
  });

  let (-->) = (a, b) => (a, b);

  let get = (key: value) => find(name(key));

  let fromList = (xs: list((string, 'a))) =>
    xs |> List.fold_left((st, (k, v)) => add(k, v, st), empty);
};

type sMap = State.t(value);

// let var = () => Var("~var:" ++ string_of_int(lvarCounter^));
let var = (name: string) => Var("~var:" ++ name);

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

let eq = (x, y, sMap) => Series.from(unify(x, y, sMap));

let and_ = (clauses: Series.t(clause), sMap): Series.t(sMap) => {
  clauses->Series.flatScan((sMap, clause) => clause(sMap), sMap);
};

let all = clauses => clauses->Series.fromList->and_;

let run = (vars: list(value), goal: clause) => {
  let sMap = State.empty;
  goal(sMap)
  ->Series.map(sMap => {
      vars |> List.map(var => (var, deepwalk(var, sMap)))
    });
};

let (=:) = eq;
