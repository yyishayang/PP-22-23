let rec sum_cifras n =
		if (n = 0) 
			then 0 
		 else (n mod 10) + sum_cifras (n/10);;
			 

let rec num_cifras n = 
		if (n<10)
			then 1
		else 1 + num_cifras(n/10);;

		
let rec exp10 n =
		if n=0
			then 1
		else 10 * exp10 (n-1);;
		
		
let reverse n = 
	let rec func n sum = 
		if n=0 
			then sum
		else func(n/10) (sum*10 + n mod 10)
	in (func n 0);;


let palindromo str = 
	let len = String.length str in
		if len <= 2 then true
		else let rec func i len =  
				if str.[i] = str.[len] 
					then (if i>=len then true
							else func (i+1)(len-1))
				else false
	in func 0 (len-1);; 

	
	
		
