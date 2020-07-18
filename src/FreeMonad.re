module type Algebra = {
  type t('a);
  let map: ('a => 'b, t('a)) => t('b);
};

module type Monad = {
  type t('a);
  let return: 'a => t('a);
  let map: ('a => 'b, t('a)) => t('b);
  let chain: ('a => t('b), t('a)) => t('b);
};

module ListMonad: Monad = {
  type t('a) = list('a);
  let return = x => [x];
  let map = List.map;
  let rec chain = (f, x) =>
    switch (x) {
    | [] => []
    | [x, ...xs] => List.append(f(x), chain(f, xs))
    };
};

module WritterMonad = {
  type t('a) = ('a, string);
  let return = x => (x, "");
  let write = (message, (x, log)) => (x, log ++ message);
  let log = ((_, log)) => log;
  let result = ((res, _)) => res;
  let map = (f, (x, log)) => (f(x), log);
  let chain = (f, (x1, log1)) => {
    let (x2, log2) = f(x1);
    (x2, log1 ++ log2);
  };
};

module Free = (A: Algebra) => {
  type free('a) =
    | Free('a)
    | Fold(A.t(free('a)));

  let pure: 'a => free('a) = x => Free(x);

  let rec map: ('a => 'b, free('a)) => free('b) =
    (f, x) =>
      switch (x) {
      | Free(x) => Free(f(x))
      | Fold(x) => Fold(A.map(map(f), x))
      };

  let rec chain: ('a => free('b), free('a)) => free('b) =
    (f, x) =>
      switch (x) {
      | Free(x) => f(x)
      | Fold(x) => Fold(A.map(chain(f), x))
      };

  let lift = x => Fold(A.map(x' => Free(x'), x));

  module Interpret = (M: Monad) => {
    let rec fold: (A.t(free('a)) => M.t(free('a)), free('a)) => M.t('a) =
      (f, x) =>
        switch (x) {
        | Free(x) => M.return(x)
        | Fold(x) => M.chain(fold(f), f(x))
        };
  };
};

module Algebra = {
  type t('a) =
    | GetString(string => 'a)
    | PutString(string, 'a)
    | Exit('a);

  let map: type a b. (a => b, t(a)) => t(b) =
    (f, x) =>
      switch (x) {
      | GetString(g) => GetString(x' => f(g(x')))
      | PutString(str, x) => PutString(str, f(x))
      | Exit(x) => Exit(f(x))
      };
};

let id = x => x;

module TestDomain = {
  include Free(Algebra);
  open Algebra;

  let (>>=) = (x, f) => chain(f, x);

  let getStr_ = lift(GetString(id));
  let putStr_ = str => lift(PutString(str, ()));
  let exit_ = lift(Exit());
};
module TestDomainInterpreter = TestDomain.Interpret(WritterMonad);

let interpreterWritterMonad = x =>
  switch (x) {
  | Algebra.GetString(f) =>
    WritterMonad.write("GetString(&&)", WritterMonad.return(f("my sting")))
  | Algebra.PutString(str, x) =>
    WritterMonad.write("PutString=" ++ str, WritterMonad.return(x))
  | Algebra.Exit(x) => WritterMonad.write("Exit", WritterMonad.return(x))
  };

let program = TestDomain.(getStr_ >>= (x => putStr_("asd" ++ x)));

let () = {
  let result = TestDomainInterpreter.fold(interpreterWritterMonad, program);
  WritterMonad.result(result);
  let _ = WritterMonad.log(result);
  ();
};