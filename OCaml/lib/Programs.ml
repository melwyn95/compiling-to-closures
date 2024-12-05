open Scheme

(* Finonacci *)
let fib_expr : expr list =
  [
    App
      ( Var "define",
        [
          Quote (Var "fib");
          Lam
            ( [ "x" ],
              If
                ( App (Var "<", [ Var "x"; Cst 2 ]),
                  Var "x",
                  App
                    ( Var "+",
                      [
                        App (Var "fib", [ App (Var "-", [ Var "x"; Cst 1 ]) ]);
                        App (Var "fib", [ App (Var "-", [ Var "x"; Cst 2 ]) ]);
                      ] ) ) );
        ] );
    App (Var "fib", [ Cst 20 ]);
  ]

(* fib_program *)

let rec fib : int -> int =
 fun x -> if x < 2 then x else fib (x - 1) + fib (x - 2)

(* Takeuchi *)
let tak_expr : expr list =
  [
    App
      ( Var "define",
        [
          Quote (Var "tak");
          Lam
            ( [ "x"; "y"; "z" ],
              If
                ( App (Var "not", [ App (Var "<", [ Var "y"; Var "x" ]) ]),
                  Var "z",
                  App
                    ( Var "tak",
                      [
                        App
                          ( Var "tak",
                            [
                              App (Var "-", [ Var "x"; Cst 1 ]);
                              Var "y";
                              Var "z";
                            ] );
                        App
                          ( Var "tak",
                            [
                              App (Var "-", [ Var "y"; Cst 1 ]);
                              Var "z";
                              Var "x";
                            ] );
                        App
                          ( Var "tak",
                            [
                              App (Var "-", [ Var "z"; Cst 1 ]);
                              Var "x";
                              Var "y";
                            ] );
                      ] ) ) );
        ] );
    App (Var "tak", [ Cst 18; Cst 12; Cst 6 ]);
  ]

(* tak_program *)

let rec tak : int -> int -> int -> int =
 fun x y z ->
  if not (y < x) then z
  else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

(* Selection Sort *)
let pie =
  [
    3;
    1;
    4;
    1;
    5;
    9;
    2;
    6;
    5;
    3;
    5;
    8;
    9;
    7;
    9;
    3;
    2;
    3;
    8;
    4;
    6;
    2;
    6;
    4;
    3;
    3;
    8;
    3;
    2;
    7;
    9;
    5;
    0;
    2;
    8;
    2;
    7;
    1;
    8;
    2;
    8;
    1;
    8;
    2;
    8;
    4;
    5;
    9;
    0;
    4;
    5;
    2;
    3;
    5;
    3;
    6;
    0;
    2;
    8;
    7;
    4;
    7;
    1;
    3;
    5;
    2;
    6;
    6;
    2;
    4;
  ]

let sort_expr : expr list =
  [
    App
      ( Var "define",
        [
          Quote (Var "sort");
          Lam
            ( [ "lst" ],
              If
                ( App (Var "null?", [ Var "lst" ]),
                  List [],
                  App
                    ( Var "sort-aux",
                      [
                        App (Var "cdr", [ Var "lst" ]);
                        List [];
                        App (Var "car", [ Var "lst" ]);
                      ] ) ) );
        ] );
    App
      ( Var "define",
        [
          Quote (Var "sort-aux");
          Lam
            ( [ "lst"; "rest"; "min" ],
              If
                ( App (Var "null?", [ Var "lst" ]),
                  App
                    (Var "cons", [ Var "min"; App (Var "sort", [ Var "rest" ]) ]),
                  If
                    ( App
                        (Var "<", [ App (Var "car", [ Var "lst" ]); Var "min" ]),
                      App
                        ( Var "sort-aux",
                          [
                            App (Var "cdr", [ Var "lst" ]);
                            App (Var "cons", [ Var "min"; Var "rest" ]);
                            App (Var "car", [ Var "lst" ]);
                          ] ),
                      App
                        ( Var "sort-aux",
                          [
                            App (Var "cdr", [ Var "lst" ]);
                            App
                              ( Var "cons",
                                [ App (Var "car", [ Var "lst" ]); Var "rest" ]
                              );
                            Var "min";
                          ] ) ) ) );
        ] );
    App (Var "sort", [ List (List.map (fun n -> Cst n) pie) ]);
  ]

let rec sort : int list -> int list =
 fun lst ->
  if List.is_empty lst then [] else sort_aux (List.tl lst) [] (List.hd lst)

and sort_aux : int list -> int list -> int -> int list =
 fun lst rest min ->
  if List.is_empty lst then min :: sort rest
  else if List.hd lst < min then
    sort_aux (List.tl lst) (min :: rest) (List.hd lst)
  else sort_aux (List.tl lst) (List.hd lst :: rest) min
