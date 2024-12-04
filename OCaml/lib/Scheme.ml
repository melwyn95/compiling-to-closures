type expr =
  | Var of string
  | Quote of expr
  | Cst of int
  | List of expr list
  | If of expr * expr * expr
  | Lam of string list * expr
  | App of expr * expr list
