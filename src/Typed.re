module StringOrd = {
  type t = string;
  let compare = (a, b) => Pervasives.compare(a, b);
};

module StringMap = Map.Make(StringOrd);

type expression =
  | EObject(list((string, expression)))
  | EArray(list(expression))
  | EString(string)
  | ENumber
  | ETrue
  | EFalse
  | ENull;

type typ =
  | TBool
  | TNumber
  | TIO
  | TList
  | TDate
  | TSet
  | TRegexp(string);

type scheme = (list(string), typ);

type envType = StringMap.t(typ);
let m: envType = StringMap.(empty |> add("a", TBool) |> add("a", TNumber));

Js.log(m);

/** Url type */
let e1 =
  EObject([
    ("postgres_url", EString("tcp://postgress@postgress:postgress/")),
  ]);

/* Password type */
let e2 = EObject([("mysql_slave_password", EString("$!L!K@L!@KJ$LKH@!$"))]);

/* Bool type */
let e3 = EObject([("is_enabled", ETrue)]);

/* Union example */
let e4 = EObject([("distributed_tracing_type", EString("opentracing"))]);

/** File type */
let e5 = EObject([("icon_path", EString("./path/to/local/file.png"))]);
let () = Js.log("typed");