open Scheme

type closure = env -> env * value

let gen_ref : string -> closure =
 fun x env ->
  match List.assoc_opt x env with
  | Some v -> (env, v)
  | None -> failwith "unbound variable"

let gen_list : closure list -> closure =
 fun cs env ->
  let vs = List.fold_left (fun vs c -> snd (c env) :: vs) [] cs in
  (env, Lst (List.rev vs))

let gen_cond : closure -> closure -> closure -> closure =
 fun cond csqt alt env ->
  if eq_value (snd (cond env)) (Q "#t") then csqt env else alt env

let gen_prc_0 : closure -> closure =
 fun body env -> (env, L0 (fun env () -> body env))

let gen_prc_1 : closure -> string -> closure =
 fun body a env -> (env, L1 (fun env x -> body ((a, x) :: env)))

let gen_prc_2 : closure -> string -> string -> closure =
 fun body a b env -> (env, L2 (fun env x y -> body ((a, x) :: (b, y) :: env)))

let gen_prc_3 : closure -> string -> string -> string -> closure =
 fun body a b c env ->
  (env, L3 (fun env x y z -> body ((a, x) :: (b, y) :: (c, z) :: env)))

let gen_app_0 : closure -> closure =
 fun f env -> match f env with _, L0 f -> f env () | _ -> failwith "not a L0"

let gen_app_1 : closure -> closure -> closure =
 fun f x env ->
  let _, f = f env in
  let _, x = x env in
  match f with L1 f -> f env x | _ -> failwith "not a L1"

let gen_app_2 : closure -> closure -> closure -> closure =
 fun f x y env ->
  let _, f = f env in
  let _, x = x env in
  let _, y = y env in
  match f with L2 f -> f env x y | _ -> failwith "not a L2"

let gen_app_3 : closure -> closure -> closure -> closure -> closure =
 fun f x y z env ->
  let _, f = f env in
  let _, x = x env in
  let _, y = y env in
  let _, z = z env in
  match f with L3 f -> f env x y z | _ -> failwith "not a L2"

let rec gen : expr -> closure =
 fun expr ->
  match expr with
  | Var x -> gen_ref x
  | Quote (Var x) -> fun env -> (env, Q x)
  | Quote _ -> failwith "quoting arbitrary expression not supported"
  | Cst c -> fun env -> (env, C c)
  | List es -> gen_list (List.map gen es)
  | If (cond, csqt, alt) -> gen_cond (gen cond) (gen csqt) (gen alt)
  | Lam ([], body) -> gen_prc_0 (gen body)
  | Lam ([ x ], body) -> gen_prc_1 (gen body) x
  | Lam ([ x; y ], body) -> gen_prc_2 (gen body) x y
  | Lam ([ x; y; z ], body) -> gen_prc_3 (gen body) x y z
  | Lam _ -> failwith "more than 3 params not supported"
  | App (f, []) -> gen_app_0 (gen f)
  | App (f, [ x ]) -> gen_app_1 (gen f) (gen x)
  | App (f, [ x; y ]) -> gen_app_2 (gen f) (gen x) (gen y)
  | App (f, [ x; y; z ]) -> gen_app_3 (gen f) (gen x) (gen y) (gen z)
  | App _ -> failwith "more than 3 params not supported"

let compile : expr list -> closure list = List.map gen

let evaluate : closure list -> value =
 fun cls -> snd @@ List.fold_left (fun (env, _) c -> c env) (init_env, U) cls
