open Scheme

let interp_var : string -> env -> value =
 fun x env -> Option.get (List.assoc_opt x env)

let rec interp : expr -> env -> env * value =
 fun expr env ->
  match expr with
  | Var x -> (env, interp_var x env)
  | Quote (Var x) -> (env, Q x)
  | Quote _ -> failwith "quoting arbitrary expression not supported"
  | Cst c -> (env, C c)
  | List xs ->
      ( env,
        Lst
          (List.rev
             (List.fold_left
                (fun ys x ->
                  let _, y = interp x env in
                  y :: ys)
                [] xs)) )
  | If (cond, csqt, alt) -> interp_cond cond csqt alt env
  | Lam ([], body) -> (env, interp_pcr_0 body)
  | Lam ([ x ], body) -> (env, interp_pcr_1 x body)
  | Lam ([ x; y ], body) -> (env, interp_pcr_2 x y body)
  | Lam ([ x; y; z ], body) -> (env, interp_pcr_3 x y z body)
  | Lam _ -> failwith "more than 3 params not supported"
  | App (f, []) -> interp_app_0 f env
  | App (f, [ x ]) -> interp_app_1 f x env
  | App (f, [ x; y ]) -> interp_app_2 f x y env
  | App (f, [ x; y; z ]) -> interp_app_3 f x y z env
  | App _ -> failwith "more than 3 params not supported"

and interp_cond : expr -> expr -> expr -> env -> env * value =
 fun cond csqt alt env ->
  let _, v1 = interp cond env in
  if eq_value v1 (Q "#t") then interp csqt env else interp alt env

and interp_pcr_0 : expr -> value =
 fun body -> L0 (fun env () -> interp body env)

and interp_pcr_1 : string -> expr -> value =
 fun a body -> L1 (fun env x -> interp body ((a, x) :: env))

and interp_pcr_2 : string -> string -> expr -> value =
 fun a b body -> L2 (fun env x y -> interp body ((a, x) :: (b, y) :: env))

and interp_pcr_3 : string -> string -> string -> expr -> value =
 fun a b c body ->
  L3 (fun env x y z -> interp body ((a, x) :: (b, y) :: (c, z) :: env))

and interp_app_0 : expr -> env -> env * value =
 fun f env ->
  match interp f env with _, L0 f -> f env () | _ -> failwith "not a L0"

and interp_app_1 : expr -> expr -> env -> env * value =
 fun f x env ->
  let _, f = interp f env in
  let _, x = interp x env in
  match f with L1 f -> f env x | _ -> failwith "not a L1"

and interp_app_2 : expr -> expr -> expr -> env -> env * value =
 fun f x y env ->
  let _, f = interp f env in
  let _, x = interp x env in
  let _, y = interp y env in
  match f with L2 f -> f env x y | _ -> failwith "not a L2"

and interp_app_3 : expr -> expr -> expr -> expr -> env -> env * value =
 fun f x y z env ->
  let _, f = interp f env in
  let _, x = interp x env in
  let _, y = interp y env in
  let _, z = interp z env in
  match f with L3 f -> f env x y z | _ -> failwith "not a L3"

let interpret : expr list -> value =
 fun es ->
  let _, v = List.fold_left (fun (env, _) e -> interp e env) (init_env, U) es in
  v
