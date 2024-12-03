module Compiler where

import Scheme

type Closure = (Env -> (Env, Value))

compile :: Expr -> Closure
compile = gen

evaluate :: [Expr] -> Value
evaluate exprs = snd $ foldl (\(env, _) e -> compile e env) (initEnv, U ()) exprs

gen :: Expr -> Closure
gen (Var x) = genRef x
gen (Quote (Var x)) = \env -> (env, Q x)
gen (Cst c) = \env -> (env, C c)
gen (List es) = genList (map gen es)
gen (If cond csqt alt) = genCond (gen cond) (gen csqt) (gen alt)
gen (Lam [] body) = genPrc0 (gen body)
gen (Lam [x] body) = genPrc1 (gen body) x
gen (Lam [x, y] body) = genPrc2 (gen body) x y
gen (Lam [x, y, z] body) = genPrc3 (gen body) x y z
gen (App f []) = genApp0 (gen f)
gen (App f [x]) = genApp1 (gen f) (gen x)
gen (App f [x, y]) = genApp2 (gen f) (gen x) (gen y)
gen (App f [x, y, z]) = genApp3 (gen f) (gen x) (gen y) (gen z)

genRef :: String -> Closure
genRef x (Env env) = case lookup x env of
  Just v -> (Env env, v)
  Nothing -> error ("Unbound variable: " ++ x)

genList :: [Closure] -> Closure
genList cs env =
  let ys =
        foldl
          (\ys c -> snd (c env) : ys)
          []
          cs
   in (env, Lst (reverse ys))

genCond :: Closure -> Closure -> Closure -> Closure
genCond cond csqt alt env = if snd (cond env) == Q "#t" then csqt env else alt env

genPrc0 :: Closure -> Closure
genPrc0 body env = (env, L0 (\env () -> body env))

genPrc1 :: Closure -> String -> Closure
genPrc1 body a env = (env, L1 (\(Env env) x -> body (Env ((a, x) : env))))

genPrc2 :: Closure -> String -> String -> Closure
genPrc2 body a b env = (env, L2 (\(Env env) x y -> body (Env ((a, x) : (b, y) : env))))

genPrc3 :: Closure -> String -> String -> String -> Closure
genPrc3 body a b c env = (env, L3 (\(Env env) x y z -> body (Env ((a, x) : (b, y) : (c, z) : env))))

genApp0 :: Closure -> Closure
genApp0 f env = let (_, L0 f0) = f env in f0 env ()

genApp1 :: Closure -> Closure -> Closure
genApp1 f x env = let (_, L1 f1) = f env in f1 env (snd (x env))

genApp2 :: Closure -> Closure -> Closure -> Closure
genApp2 f x y env = let (_, L2 f2) = f env in f2 env (snd (x env)) (snd (y env))

genApp3 :: Closure -> Closure -> Closure -> Closure -> Closure
genApp3 f x y z env = let (_, L3 f3) = f env in f3 env (snd (x env)) (snd (y env)) (snd (z env))
