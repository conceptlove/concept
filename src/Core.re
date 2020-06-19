let apply = (f, x) => f(x);
let identity = x => x;
let always = (x, _) => x;

let trace = (tag, v) => {
  Js.log2(tag ++ ":", v);
  v;
};
