let rec fib n =
		if n <= 1 then n
		else fib (n-1) + fib (n-2)
				  		
						
let rec print x = if (x=0) 
	  				then ()
  				else 
  					print (x-1);
  					let rtdo = fib x in print_endline (string_of_int rtdo)
					

let () = if (Array.length Sys.argv == 2)
			then let max = (int_of_string Sys.argv.(1)) in print max
		else 
		  	print_endline "Error: argumento inválido"
