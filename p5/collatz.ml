let f n = 
	if n mod 2 = 0 
		then n / 2 
	else 3 * n + 1;;

let rec orbit n =
	if(n=1)
		then "1"
	else
		(string_of_int n)^", "^orbit (f n);;
	
	
let rec length n = 
	if (f n= 1) 
		then 1
	else 1+ length (f n);;
	
	
let top n =
	let rec g n max = 
		let aux = n in 
			if (n=1)
				then max
			else if (aux > max)  
				then (g (f n)) aux
			else (g (f n)) max
	in (g n) 1;;
	
(*let rec top n =
	if n=1
		then 1
	else max n (top (f n))*)
	
	
let rec length'n'top n = 
	if n=1
		then (0,1)
	else 
		let len,top = length'n'top (f n) in 
			(1+len, max n top);;
	
			
let longest_in m n = 
	let rec g n max = 
		if (n<m)
			then (max, length max)
		else
			let l_max = length max in 
				if(l_max >= length n)
					then g (n-1) max
				else 
					g (n-1) n
	in g n m;;
	

let highest_in m n = 
	let rec g n max = top
		if (n<m)
			then (max, top max)
		else
			let h_max = top max in 
				if(h_max >= top n)
					then g (n-1) max
				else 
					g (n-1) n
	in g n m;;
			
