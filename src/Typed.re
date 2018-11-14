type expression =
  | EObject(list((key, expression)))
  | EArray(list(expression))
  | EString
  | ENumber
  | ETrue
  | EFalse
  | ENull
and key = string;

let () = Js.log("typed");