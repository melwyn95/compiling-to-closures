module Compiler where

import Scheme

type Closure = (Env -> Value)

compile :: Expr -> Closure
compile = gen

evaluate :: [TopExpr] -> Value
evaluate exprs =
  let (Env init) = initEnv
   in let topDefs =
            [ d | t <- exprs, isDefine t, let d =
                                                let Define f v = t
                                                    Env e = initEnv
                                                    v' = compile v (Env ((f, v') : topDefs ++ e))
                                                 in (f, v')
            ]
              ++ init
       in let (env, v) =
                foldl
                  ( \(Env env, _) expr -> case expr of
                      Define f e -> let v = compile e (Env env) in (Env ((f, v) : env), v)
                      Expr e -> (Env env, compile e (Env env))
                  )
                  (Env topDefs, U ())
                  exprs
           in v

gen :: Expr -> Closure
gen (Var x) = genRef x
gen (Quote (Var x)) = \_ -> Q x
gen (Cst c) = \_ -> C c
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
  Just v -> v
  Nothing -> error ("Unbound variable: " ++ x)

genList :: [Closure] -> Closure
genList es env = Lst $ map (\ce -> ce env) es

genCond :: Closure -> Closure -> Closure -> Closure
genCond cond csqt alt env = if cond env == Q "#t" then csqt env else alt env

genPrc0 :: Closure -> Closure
genPrc0 body env = L0 (\() -> body env)

genPrc1 :: Closure -> String -> Closure
genPrc1 body a (Env env) = L1 (\x -> body (Env ((a, x) : env)))

genPrc2 :: Closure -> String -> String -> Closure
genPrc2 body a b (Env env) = L2 (\x y -> body (Env ((a, x) : (b, y) : env)))

genPrc3 :: Closure -> String -> String -> String -> Closure
genPrc3 body a b c (Env env) = L3 (\x y z -> body (Env ((a, x) : (b, y) : (c, z) : env)))

genApp0 :: Closure -> Closure
genApp0 f env = let (L0 f0) = f env in f0 ()

genApp1 :: Closure -> Closure -> Closure
genApp1 f x env = let (L1 f1) = f env in f1 (x env)

genApp2 :: Closure -> Closure -> Closure -> Closure
genApp2 f x y env = let (L2 f2) = f env in f2 (x env) (y env)

genApp3 :: Closure -> Closure -> Closure -> Closure -> Closure
genApp3 f x y z env = let (L3 f3) = f env in f3 (x env) (y env) (z env)
