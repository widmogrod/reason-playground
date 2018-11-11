Js.log("Hello, BuckleScript and Reason!");
let ageAndName: (int, string) = (33, "Gabriel");
type person = {
  age: int,
  name: string,
};
let me = {age: 5, name: "Big Reason"};

type paymentMethod =
  | Cash
  | MasterCard;

type shoppingCart =
  | Empty
  | ActiveCart(list(string))
  | PayedCart(list(string), paymentMethod);

let addToCart = (c, i) =>
  switch (c) {
  | Empty => ActiveCart([i])
  | ActiveCart(items) => ActiveCart([i, ...items])
  | PayedCart(_) => c
  };

let c = addToCart(Empty, "iPhone XR");
Js.log(c);