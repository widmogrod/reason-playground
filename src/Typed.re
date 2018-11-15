module StringOrd = {
  type t = string;
  let compare = (a, b) => Pervasives.compare(a, b);
};

module StringMap = Map.Make(StringOrd);

type expression =
  | EObject(StringMap.t(expression))
  /* | EArray(list(expression)) */
  | EString(string)
  /* | ENumber */
  | ETrue
  | EFalse;
/* | ENull; */

type typ =
  /* | TBool */
  /* | TNumber */
  | TIO
  /* | TList */
  /* | TDate */
  /* | TSet */
  | TPassword
  | TRecord(StringMap.t(typ))
  | TLit(lit)
  | TSum(typ, typ)
  | TRegexp(string)
  | TDefined(string, typ)
and lit =
  | LInt
  | LBool
  | LString(string); /* This should be more like atom/const */

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
    |> add("_enabled$", TLit(LBool))
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
  EObject(
    StringMap.(
      empty
      |> add("postgres_url", EString("tcp://postgress@postgress:postgress/"))
    ),
  );

let r1 = "{ postgres_url = _url of regexp}";

/* Password type */
let e2 =
  EObject(
    StringMap.(
      empty |> add("mysql_slave_password", EString("$!L!K@L!@KJ$LKH@!$"))
    ),
  );
let r2 = "{mysql_slave_password = _password of password}";

/* Bool type */
let e3 = EObject(StringMap.(empty |> add("is_enabled", ETrue)));
let r3 = "{is_enabled = _enabled of bool}";

/* Union example */
let e4 =
  EObject(
    StringMap.(
      empty |> add("distributed_tracing_type", EString("opentracing"))
    ),
  );
let r4 = "{distributed_tracing_type = _tracing_type of sum of string [opentracing, jaeger, none]}";

/** File type */
let e5 =
  EObject(
    StringMap.(
      empty |> add("icon_path", EString("./path/to/local/file.png"))
    ),
  );
let r5 = "{icon_path = _paht of io}";

let rec typeInference = (e: expression) =>
  switch (e) {
  | EFalse => TLit(LBool)
  | ETrue => TLit(LBool)
  | EString(v) => TLit(LString(v))
  | EObject(map) => TRecord(StringMap.map(typeInference, map))
  };

let rec showType = t =>
  switch (t) {
  | TIO => "io"
  | TPassword => "password"
  | TLit(l) =>
    switch (l) {
    | LInt => "int"
    | LBool => "bool"
    | LString(_) => "string"
    }
  | TSum(_, _) => "sum"
  | TDefined(dt, _) => dt
  | TRegexp(_) => "regexp"
  | TRecord(map) =>
    Format.sprintf(
      "{%s}",
      StringMap.fold(
        (k, v, agg) => Format.sprintf("%s %s = %s", agg, k, showType(v)),
        map,
        "",
      ),
    )
  };

let test_it = p =>
  switch (p) {
  | (e: expression, r: string) =>
    Js.log3("run expression of", e, r);
    Js.log2("expected:", showType(typeInference(e)));
    assert(showType(typeInference(e)) == r);
  };

let () = Js.log("typed");

[(e1, r1)] |> List.map(test_it);