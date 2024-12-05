open Lib.Programs
open Lib.Scheme

(* Finonacci *)
let fib_program = Lib.Compiler.compile fib_expr
let () = assert (eq_value (C (fib 20)) (Lib.Interpreter.interpret fib_expr))
let () = assert (C (fib 20) = Lib.Compiler.evaluate fib_program)

(* Takeuchi *)
let tak_program = Lib.Compiler.compile tak_expr

let () =
  assert (eq_value (C (tak 18 12 6)) (Lib.Interpreter.interpret tak_expr))

let () = assert (eq_value (C (tak 18 12 6)) (Lib.Compiler.evaluate tak_program))

(* Selection Sort *)
let sort_program = Lib.Compiler.compile sort_expr

let () =
  assert (
    eq_value
      (Lst (List.map (fun n -> C n) @@ sort pie))
      (Lib.Interpreter.interpret sort_expr))

let () =
  assert (
    eq_value
      (Lst (List.map (fun n -> C n) @@ sort pie))
      (Lib.Compiler.evaluate sort_program))

let () = print_endline "All Tests Passed!"
