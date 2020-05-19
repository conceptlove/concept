open Fact;

type t('a) =
  {
    ..
    id: T.str,
    name: T.str,
  } as 'a;
