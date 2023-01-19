
open Parsing;;
open Lexing;;

open Context;;
open Lib;;
open Arith;;
open Command;;
open Parser;;
open Lexer;;

let rec loop ctx =
    print_string ">> ";

    let newctx = try 
        let cmd = s token (from_string (read_line ())) in
			run ctx cmd 
    with 
        No_binding s -> print_endline ("Variable "^s^" no definida"); ctx
        | Function_not_defined f -> print_endline ("Función "^f^" no definida"); ctx
        | Lexical_error -> print_endline "Error léxico"; ctx
        | Parse_error -> print_endline "Error sintáctico"; ctx

in loop newctx;;

let _ = print_endline "Floating point calculator..." in
let _ = 
	try loop empty_context
    with End_of_program -> () 
in print_endline "... bye!!!";;


