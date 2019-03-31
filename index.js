// http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf

const ADT = require("@masaeedu/adt");
const { adt, match } = ADT;
const { Fn } = require("@masaeedu/fp");

// :: type Functor f = { map: (a -> b) -> f a -> f b }

// :: type Applicative f =
// ::   (Functor f)
// :: & { pure: a -> f a }
// :: & (
// ::     { lift2: (a -> b -> c) -> f a -> f b -> f c }
// ::   | { ap: f (a -> b) -> f a -> f b }
// ::   )

// :: type Either a b = Case "Left" [a] | Case "Right" [b]

const Either = (() => {
  const { Left, Right } = adt({ Left: ["a"], Right: ["b"] });

  // :: (a -> c) -> Either a b -> Either c b
  const lmap = f => match({ Left: Fn.pipe([f, Left]), Right });

  // :: (b -> c) -> Either a b -> Either a c
  const rmap = f => match({ Left, Right: Fn.pipe([f, Right]) });

  return { Left, Right, lmap, rmap };
})();
const { Left, Right } = Either;

// :: type Maybe a = Case "Just" [a] | Case "Nothing" []

const Maybe = (() => {
  const { Nothing, Just } = adt({ Nothing: [], Just: ["a"] });

  return { Nothing, Just };
})();
const { Nothing, Just } = Maybe;

// :: type Lens a b s t = { view: s -> a, update: [b, s] -> t }

const Lens = (() => {
  // :: Lens a b s t -> s -> a
  const view = L => s => L.view(s);

  // :: Lens a b s t -> b -> s -> t
  const update = L => b => s => L.update([b, s]);

  // :: Lens a b [a, c] [b, c]
  const left = {
    view: ([a, c]) => a,
    update: ([b, [_, c]]) => [b, c]
  };

  // :: Lens Bool Bool Int Int
  const sign = {
    view: x => x >= 0,
    update: ([b, x]) => (b ? Math.abs(x) : -Math.abs(x))
  };

  console.log(update(sign)(false)(5));
  // => -5

  return { view, update, left, sign };
})();

// :: type Prism a b s t = { match: s -> Either t a, build: b -> t }

const Prism = (() => {
  // :: Prism a b (Maybe a) (Maybe b)
  const the = {
    match: match({ Nothing: Left(Nothing), Just: Right }),
    build: Just
  };

  // :: Prism Integer Integer Double Double
  const whole = {
    match: x => {
      const f = x % 1;
      return f === 0 ? Right(x - f) : Left(x);
    },
    build: x => x
  };

  console.log([
    whole.match(25.5),
    // => Left(25.5)

    whole.match(42)
    // => Right(42)
  ]);

  return { the, whole };
})();

// ### ASIDE { ###
//
// // Was wondering whether the Lens can itself be a profunctor as-is, and indeed it can
//
// // :: type Profunctor p = { dimap: (a' -> a) -> (b -> b') -> p a b -> p a' b' }
//
// // :: Profunctor (Lens a b)
// const dimap = bck => fwd => ({ view, update }) => {
//   // :: bck :: s' -> s
//   // :: fwd :: t -> t'
//   // :: view :: s -> a
//   // :: update :: [b, s] -> t
//
//   // :: s' -> a
//   const view_ = Fn.compose(view)(bck);
//   // :: [b, s'] -> t'
//   const update_ = ([b, s_]) => fwd(update([b, bck(s_)]));
//
//   return { view: view_, update: update_ };
// };
//
// ### } ASIDE ###

// :: type Adapter a b s t = { from: s -> a, to: b -> t }

const Adapter = (() => {
  // :: Adapter [a, b, c] [a', b', c'] [[a, b], c] [[a', b'], c']
  const flatten = {
    from: ([[x, y], z]) => [x, y, z],
    to: ([x, y, z]) => [[x, y], z]
  };

  return { flatten };
})();

const { Empty, Node } = adt({ Empty: [], Node: ["Tree a", "a", "Tree a"] });

const Tree = (() => {
  // :: Applicative f -> (a -> f b) -> Tree a -> f (Tree b)
  const inorder = A => f => {
    const rec = match({
      Empty: A.pure(Empty),
      Node: t => x => u => A.lift(Node)([rec(t), f(x), rec(u)])
    });

    return rec;
  };

  return { Empty, Node, inorder };
})();

// :: type Case l vs = { label: l, values: vs }

// :: type FunList a b t = Case "Done" [t] | Case "More" [a, FunList a b (b -> t)]

// Expanded out, that's essentially
// type FunList a b t = t | [a, FunList a b (b -> t)]
//                    = t | [a, b -> t | [a, FunList a b (b -> b -> t)]]
//                    = t | [a, b -> t | [a, b -> b -> t | [a, FunList a b (b -> b -> b -> t)]]]
//                    = t | [a, b -> t | [a, b -> b -> t | [a, b -> b -> b -> t | [a, FunList a b (b -> b -> b -> b -> t)]]]]
//                    = ...

// ### ASIDE { ###

// Bartosz Milewski has a post about how this recursive functor is the initial algebra
// of a weird functor that involves the indexed store comonad somehow

// :: type Store a b s = [a, b -> s]
// :: type Day f g s = [f a, g b, [a, b] -> s]
// :: type FunList a b t = Case "Done" [t] | Case "More" [Day (Store a b) (FunList a b) t]
// ::                    = (Identity :+: Day (Store a b) (FunList a b)) t
// :: type (:+:) f g t = Case "Left" [f t] | Case "Right" [g t]

// ### } ASIDE ###

const FunList = adt({ Done: ["t"], More: ["a", "FunList a b (b -> t)"] });
const { Done, More } = FunList;

// :: FunList a b t -> Either t [a, FunList a b (b -> t)]
const out = match({
  Done: Left,
  More: x => l => Right([x, l])
});

// :: Either t [a, FunList a b (b -> t)] -> FunList a b t
const inn = match({
  Left: Done,
  Right: ([x, l]) => More(x)(l)
});

// :: Functor (FunList a b)
const FLF = {
  map: f =>
    match({
      Done: t => Done(f(t)),
      More: x => l => More(x)(FLF.map(Fn.compose(f))(l))
    })
};

// :: Applicative (FunList a b)
const FLA = {
  ...FLF,
  pure: Done,
  ap: match({
    Done: f => l_ => FLF.map(f)(l_),
    More: x => l => l_ => More(x)(FLA.ap(FLF.map(Fn.flip)(l))(l_))
  })
};

// :: a -> FunList a b b
const single = x => More(x)(Done(Fn.id));

// :: FunList b b t -> t
const fuse = match({
  Done: t => t,
  More: x => l => fuse(l)(x)
});

// :: type Traversal a b s t = { extract: s -> FunList a b t }

// :: Traversal a b (Tree a) (Tree b)
const inorderC = {
  extract: Tree.inorder(FLA)(single)
};

// :: type Profunctor p = { dimap: (a' -> a) -> (b -> b') -> p a b -> p a' b' }

// :: Profunctor (->)
const FnP = {
  dimap: f => g => h => Fn.pipe([f, h, g])
};

// :: type UpStar f a b = a -> f b

// :: Functor f -> Profunctor (Upstar f)
const FUSP = F => ({
  dimap: f => g => h => Fn.pipe([f, h, F.map(g)])
});

// :: type Cartesian p =
// ::   (Profunctor p)
// :: & (
// ::     { first:  p a b -> p [a, c] [b, c] }
// ::   | { second: p a b -> p [c, a] [c, b] }
// ::   )

const Pair = (() => {
  // :: [a, b] -> [b, a]
  const flip = ([a, b]) => [b, a];

  return { flip };
})();

// :: Cartesian p -> p a b -> p [c, a] [c, b]
const second = C => p => C.dimap(Pair.flip)(Pair.flip)(C.first(p));

// :: Cartesian (->)
const FnC = {
  ...FnP,
  first: h => ([a, c]) => [h(a), c]
};

// :: Functor f -> Cartesian (Upstar f)
const FUSC = F => ({
  ...FUSP(F),
  first: h => ([a, c]) => F.map(b => [b, c])(h(a))
});

// :: type CoCartesian p =
// ::   (Profunctor p)
// :: & (
// ::     { left:  p a b -> p (Either a c) (Either b c) }
// ::   | { right: p a b -> p (Either c a) (Either c b) }
// ::   )

// :: Either a b -> Either b a
Either.flip = match({ Left: Right, Right: Left });

// :: CoCartesian p -> p a b -> p (Either c a) (Either c b)
const right = CoC => p => CoC.dimap(Either.flip)(Either.flip)(CoC.left(p));

// :: CoCartesian (->)
const FnCoC = {
  ...FnP,
  left: h => match({ Left: x => Left(h(x)), Right })
};

// :: Applicative f -> CoCartesian (Upstar f)
const AUSCoC = A => ({
  ...FUSP(A),
  // :: (a -> f b) -> Either a c -> f (Either b c)
  left: h =>
    match({ Left: a => A.map(Left)(h(a)), Right: c => A.pure(Right(c)) })
});

// :: type Monoidal p =
// ::   (Profunctor p)
// :: & { par:   p a b -> p c d -> p [a, c] [b, d]
// ::   , empty: p Unit Unit                       }

// :: Monoidal (->)
const FnMon = {
  ...FnP,
  par: f => g => ([a, c]) => [f(a), g(c)],
  empty: Fn.id
};

// :: Applicative f -> Monoidal (Upstar f)
const AUSMon = A => ({
  ...FUSP(A),
  // :: (a -> f b) -> (c -> f d) -> [a, c] -> f [b, d]
  par: afb => cfd => ([a, c]) => A.lift2(Fn.pair)(afb(a))(cfd(c)),
  // :: Unit -> f Unit
  empty: A.pure
});

// :: type Optic p a b s t = p a b -> p s t

// :: type AdapterP a b s t = Profunctor p -> Optic p a b s t

// :: Adapter a b s t -> AdapterP a b s t
const adapterC2P = ({ to, from }) => P => P.dimap(from)(to);

// :: Profunctor (Adapter a b)
const AP = {
  // :: (s' -> s) -> (t -> t') -> Adapter a b s t -> Adapter a b s' t'
  dimap: f => g => ({ to: to_, from: from_ }) => {
    // :: s' -> a
    const to = s_ => to_(f(s_));

    // :: b -> t'
    const from = b => g(from_(b));

    return { to, from };
  }
};

// :: AdapterP a b s t -> Adapter a b s t
const adapterP2C = o => o(AP)({ to: Fn.id, from: Fn.id });

// :: type LensP a b s t = Cartesian p -> Optic p a b s t

// :: Profunctor (Lens a b)
const LP = {
  // :: (s' -> s) -> (t -> t') -> Lens a b s t -> Lens a b s' t'
  dimap: f => g => ({ view: v, update: u }) => {
    // :: s' -> a
    const view = s_ => v(f(s_));

    // :: [b, s'] -> t'
    const update = ([b, s_]) => g(u([b, f(s_)]));

    return { view, update };
  }
};

// :: Cartesian (Lens a b)
const LC = {
  ...LP,

  // :: Lens a b s t -> Lens a b [s, c] [t, c]
  first: ({ view: view_, update: update_ }) => {
    // :: [s, c] -> a
    const view = ([s, _]) => view_(s);

    // :: [b, [s, c]] -> [t, c]
    const update = ([b, [s, c]]) => [update_([b, s]), c];

    return { view, update };
  }
};

// :: Lens a b s t -> LensP a b s t
const lensC2P = ({ view, update }) => P => l =>
  P.dimap(x => [view(x), x])(update)(P.first(l));

// :: LensP a b s t -> Lens a b s t
const lensP2C = o => o(LP)({ view: Fn.id, update: ([b, _]) => b });

// :: type PrismP a b s t = CoCartesian p -> Optic p a b s t

// :: Profunctor (Prism a b)
const PrP = {
  // :: (s' -> s) -> (t -> t') -> Prism a b s t -> Prism a b s' t'
  dimap: f => g => ({ match: m, build: b }) => {
    // :: s' -> Either t' a
    const match = Fn.pipe([f, m, ADT.match({ Right, Left: t => Left(g(t)) })]);

    // :: b -> t'
    const build = Fn.pipe([b, g]);

    return { match, build };
  }
};

// :: (a -> x) -> (b -> x) -> Either a b -> x
const either = f => g => ADT.match({ Left: f, Right: g });

// :: CoCartesian (Prism a b)
const PrCoC = {
  ...PrP,

  // :: Prism a b s t -> Prism a b (Either s c) (Either t c)
  left: ({ match: m, build: b }) => {
    // :: (Either s c) -> Either (Either t c) a
    const match = ADT.match({
      // :: s -> Either (Either t c) a
      Left: Fn.pipe([m, Either.lmap(Left)]),

      // :: c -> Either (Either t c) a
      Right: Fn.pipe([Right, Left])
    });

    // :: b -> Either t c
    const build = Fn.pipe([b, Left]);

    return { match, build };
  }
};

// Isomorphisms yada yada yada
// :: Prism a b s t -> PrismP a b s t
prismC2P = undefined;

// :: PrismP a b s t -> Prism a b s t
prismP2C = undefined;
