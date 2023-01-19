
let rec fact = function
	0 -> 1 
  | n -> n * fact (n - 1);;
  

let () = try if Array.length Sys.argv == 2 
			then let num = int_of_string Sys.argv.(1) in 
				let rtdo = fact num in 
					print_endline (string_of_int rtdo)
		 with 
		    Failure _ |Stack_overflow -> print_string "fact: argumento inválido\n"
