import Compiler
import Criterion.Main
import Interpreter
import Programs

main :: IO ()
main =
  defaultMain
    [ let compiledProgram = compile fibExpr
       in bgroup
            "fib"
            [ bench "impl-lang" $ whnf fib 20,
              bench "interpreter" $ whnf interpret fibExpr,
              bench "compiler" $ whnf evaluate $! compiledProgram
            ],
      let compiledProgram = compile takExpr
       in bgroup
            "tak"
            [ bench "impl-lang" $ whnf (\(a, b, c) -> tak a b c) (18, 12, 6),
              bench "interpreter" $ whnf interpret takExpr,
              bench "compiler" $ whnf evaluate $! compiledProgram
            ],
      let compiledProgram = compile sortExpr
       in bgroup
            "sort"
            [ bench "impl-lang" $ whnf sort pie,
              bench "interpreter" $ whnf interpret sortExpr,
              bench "compiler" $ whnf evaluate $! compiledProgram
            ]
    ]
