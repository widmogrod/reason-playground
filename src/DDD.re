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

let axioms = (add, a, b, c) => {
  let c = Empty;
  assert(c == c);
  assert(add(c, a) == add(c, a));
  assert(add(add(c, a), a) == add(c, a));
  /* assert(add(add(c, a), b) == add(add(c, b), a)); */
};

let () = axioms(addToCart, "iPhone XR", "iPad", "Mac");