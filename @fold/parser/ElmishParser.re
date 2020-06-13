type located('ctx) = {
  row: int,
  col: int,
  ctx: 'ctx,
};

type state('ctx) = {
  src: string,
  offset: int,
  indent: int,
  ctx: list(located('ctx)),
};

type deadEnd('ctx, 'problem) = {
  row: int,
  col: int,
  problem: 'problem,
  ctxStack: list(located('ctx)),
};

type bag('c, 'x) =
  | Empty
  | AddRight(bag('c, 'x), deadEnd('c, 'x))
  | Append(bag('c, 'x), bag('c, 'x));

type pStep('ctx, 'problem, 'value) =
  | Good(bool, 'value, state('ctx))
  | Bad(bool, bag('ctx, 'problem));

type parser('ctx, 'problem, 'value) =
  | Parser(state('ctx) => pStep('ctx, 'problem, 'value));
