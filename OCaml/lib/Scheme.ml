type expr =
  | Var of string
  | Quote of expr
  | Cst of int
  | List of expr list
  | If of expr * expr * expr
  | Lam of string list * expr
  | App of expr * expr list
[@@deriving show, eq]

type value =
  | Q of string
  | C of int
  | U
  | Lst of value list
  | L0 of (env -> unit -> env * value)
  | L1 of (env -> value -> env * value)
  | L2 of (env -> value -> value -> env * value)
  | L3 of (env -> value -> value -> value -> env * value)
[@@deriving show]

and env = (string * value) list [@@deriving show]

let rec eq_value : value -> value -> bool =
 fun x y ->
  match (x, y) with
  | Q x, Q y -> x = y
  | C x, C y -> x = y
  | U, U -> true
  | Lst xs, Lst ys ->
      List.for_all (fun (x, y) -> eq_value x y) (List.combine xs ys)
  | _ -> false

let[@warning "-8"] init_env : env =
  [
    ("cons", L2 (fun env x (Lst xs) -> (env, Lst (x :: xs))));
    ("car", L1 (fun env (Lst xs) -> (env, List.hd xs)));
    ("cdr", L1 (fun env (Lst xs) -> (env, Lst (List.tl xs))));
    ( "null?",
      L1
        (fun env (Lst xs) -> (env, if List.is_empty xs then Q "#t" else Q "#f"))
    );
    ( "not",
      L1
        (fun env x ->
          ( env,
            match x with Q "#t" -> Q "#f" | Q "#f" -> Q "#t" | _ -> Q "#void" ))
    );
    ( "<",
      L2 (fun env (C c1) (C c2) -> (env, if c1 < c2 then Q "#t" else Q "#f")) );
    ("+", L2 (fun env (C c1) (C c2) -> (env, C (c1 + c2))));
    ("-", L2 (fun env (C c1) (C c2) -> (env, C (c1 - c2))));
    ("define", L2 (fun env (Q f) v -> ((f, v) :: env, U)));
  ]
