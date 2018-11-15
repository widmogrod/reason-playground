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
  | TPassword
  | TLit(lit)
  | TSum(typ, typ)
  | TRegexp(string)
  | TDefined(string, typ)
and lit =
  | LInt(int)
  | LBool(bool)
  | LString(string);

type scheme =
  | Scheme(list(string), typ);

type envType = StringMap.t(typ);
/*

 type match _url$ = Regexp(#\w+://.+#i)
 type sufix _url = Regexp(#\w+://.+#i)

 {
   "__types__": {
     "sufix _url":["Regexp", "#\w+://.+#i"],
   },
 }

  */

let m: envType =
  StringMap.(
    empty
    |> add("_url$", TDefined("_url", TRegexp("#\w+://.+#i")))
    |> add("_enabled$", TBool)
    |> add("_password$", TPassword)
    |> add("_path$", TIO)
    |> add(
         "_tracing_type$",
         TSum(
           TLit(LString("jaeger")),
           TSum(TLit(LString("opentracing")), TLit(LString("none"))),
         ),
       )
  );

Js.log(m);

/** Url type */
let e1 =
  EObject([
    ("postgres_url", EString("tcp://postgress@postgress:postgress/")),
  ]);

let r1 = "{postgres_url = _url of regexp}";

/* Password type */
let e2 = EObject([("mysql_slave_password", EString("$!L!K@L!@KJ$LKH@!$"))]);
let r2 = "{mysql_slave_password = _password of password}";

/* Bool type */
let e3 = EObject([("is_enabled", ETrue)]);
let r3 = "{is_enabled = _enabled of bool}";

/* Union example */
let e4 = EObject([("distributed_tracing_type", EString("opentracing"))]);
let r4 = "{distributed_tracing_type = _tracing_type of sum of string [opentracing, jaeger, none]}";

/** File type */
let e5 = EObject([("icon_path", EString("./path/to/local/file.png"))]);
let r5 = "{icon_path = _paht of io}";

let () = Js.log("typed");