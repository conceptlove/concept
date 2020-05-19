// open Fact;

type named('a) = {.. name: string} as 'a;

type t('a) = {.. x: string} as 'a;

type x('a) = t(named('a));
