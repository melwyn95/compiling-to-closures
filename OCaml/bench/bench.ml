open Core_bench
open Lib.Programs
open Lib.Scheme

let fib =
  Bench.Test.create_group ~name:"fib"
    [
      Bench.Test.create ~name:"impl-lang" (fun () -> C (fib 20));
      Bench.Test.create ~name:"interpreter" (fun () ->
          Lib.Interpreter.interpret fib_expr);
      (let fib_program = Lib.Compiler.compile fib_expr in
       Bench.Test.create ~name:"compiler" (fun () ->
           Lib.Compiler.evaluate fib_program));
    ]

let tak =
  Bench.Test.create_group ~name:"tak"
    [
      Bench.Test.create ~name:"impl-lang" (fun () -> C (tak 18 12 6));
      Bench.Test.create ~name:"interpreter" (fun () ->
          Lib.Interpreter.interpret tak_expr);
      (let tak_program = Lib.Compiler.compile tak_expr in
       Bench.Test.create ~name:"compiler" (fun () ->
           Lib.Compiler.evaluate tak_program));
    ]

let sort =
  Bench.Test.create_group ~name:"sort"
    [
      Bench.Test.create ~name:"impl-lang" (fun () ->
          Lst (List.map (fun n -> C n) @@ sort pie));
      Bench.Test.create ~name:"interpreter" (fun () ->
          Lib.Interpreter.interpret sort_expr);
      (let sort_program = Lib.Compiler.compile sort_expr in
       Bench.Test.create ~name:"compiler" (fun () ->
           Lib.Compiler.evaluate sort_program));
    ]

let tests = [ fib; tak; sort ]
let () = Command_unix.run (Bench.make_command tests)
