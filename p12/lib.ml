
exception Function_not_defined of string;;

(*let get_function s = match s with 
	 "sqrt" -> sqrt
	|"exp" -> exp
	|"ln" -> log
	|"round" -> function x -> floor (x +. 0.5)
	| _ -> raise (Function_not_defined s);;

*)

let funs =
[("sqrt", Float.sqrt);
 ("exp", Float.exp);
 ("ln", Float.log);
 ("round", Float.round)];;

let get_function s = 
	try List.assoc s funs 
	with Not_found -> raise (Function_not_defined s);;
