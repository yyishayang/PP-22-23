
open Context;;
open Lib;;

type arith_oper =
    Opp;;

type arith_bi_oper =
    Sum | Sub | Prod | Div | Mod | Pow;;
    

let get_arith_bi_oper oper = match oper with 
	Sum -> (+.)
	|Sub -> (-.) 
	|Prod -> ( *.)
	|Div -> (/.)
	|Mod -> (fun f1 f2 -> f1 -. floor ( f1 /.f2) ) 
	|Pow -> ( **)

type arith =
    Float of float
  | Var of string
  | Arith_op of arith_oper * arith
  | Arith_bi_op of arith_bi_oper * arith * arith
  | Fun_call of string * arith;;

let rec eval ctx = function
    Float f -> f
  | Var name -> get_binding ctx name
  | Arith_op (Opp, op) -> -. (eval ctx op)
  | Arith_bi_op (operator, op1, op2) -> (get_arith_bi_oper operator) (eval ctx op1) (eval ctx op2)
  | Fun_call (name, op) -> (get_function name) (eval ctx op);;
  
 

