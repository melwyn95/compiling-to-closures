import Compiler
import Criterion.Main
import Interpreter
import Programs

main :: IO ()
main =
  let compiledProgram = compile fibExpr
   in defaultMain
        [ bgroup
            "fib"
            [ bench "impl-lang" $ whnf fib 20,
              bench "interpreter" $ whnf interpret fibExpr,
              bench "compiler" $ whnf evaluate compiledProgram
            ]
        ]
