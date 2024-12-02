import Compiler
import Criterion.Main
import Interpreter
import Programs

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bench "impl-lang" $ whnf fib 1,
          bench "interpreter" $ whnf interpret fibExpr,
          bench "compiler" $ whnf evaluate fibExpr
        ]
    ]