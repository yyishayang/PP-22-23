
open Context;;
open Arith;;

exception End_of_program;;

type command =
    Eval of arith
  | Var_def of string * arith
  | Quit;;

let run ctx = function
    Eval e ->
      let f = eval ctx e in
      let _ = print_endline (string_of_float f) in
      ctx
  | Var_def (s, e) -> add_binding ctx s (eval ctx e)
  | Quit -> raise (End_of_program);;
