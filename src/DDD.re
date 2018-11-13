module ProductList = {
  type t = string;
  let compare = (a, b) => Pervasives.compare(a, b);
};

module Products = Set.Make(ProductList);

type paymentMethod =
  | Cash
  | MasterCard;

type shoppingCart =
  | Empty
  | ActiveCart(Products.t)
  | PayedCart(Products.t, paymentMethod);

let addToCart = (c, i) =>
  switch (c) {
  | Empty => ActiveCart(Products.(empty |> add(i)))
  | ActiveCart(items) => ActiveCart(Products.add(i, items))
  | PayedCart(_) => c
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Empty, Empty) => true
  | (ActiveCart(aa), ActiveCart(bs)) => Products.equal(aa, bs)
  | (PayedCart(aa, am), PayedCart(bs, bm)) =>
    am == bm && Products.equal(aa, bs)
  | (_, _) => false
  };

let axioms = (add, a, b) => {
  let c = Empty;
  /* Empty carts are the same */
  assert(equal(c, c));
  /* Adding same items to two cars, means that they are equal */
  assert(equal(add(c, a), add(c, a)));
  /* Adding two different items to cart, means that they are different */
  assert(!equal(add(c, a), add(c, b)));
  /* Indepotent. Adding two times, same item into cart means that carts are the same */
  assert(equal(add(add(c, a), a), add(c, a)));
  /* Commutative. Adding two items to the carts in different order results in the same cart */
  assert(equal(add(add(c, a), b), add(add(c, b), a)));
};

let () = axioms(addToCart, "iPhone", "Mac");