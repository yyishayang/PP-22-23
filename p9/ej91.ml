(*let rec to0from n =
	if n < 0 then [] else n :: to0from (n-1);; *)

let to0from n = 
	let rec aux n l = 
		if n<0 then List.rev l
		else aux (n-1) (n::l)
	in aux n [];;
	
	
(*let rec fromto m n =
	if m > n then [] else m :: fromto (m+1) n;;*)

let fromto m n =
	let rec aux m n l = 
		if m>n then List. rev l
		else aux (m+1) n (m::l)
	in aux m n [];; 


(*let incseg l =
List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;*)

let incseg l = 
	let rec aux l l1 sum = match l with 
		[] -> []
		|[h] -> List.rev ((h+sum)::l1)
		|h::t -> aux t ((h+sum)::l1) (h+sum)
	in aux l [] 0;;
		 


(*let rec remove x = function
	[] -> [] 
	| h::t -> if x = h then t else h :: remove x t;;*)

let remove x l = 
	let rec aux x l l1 = match l with 
		[] -> l1
		|h::t -> if x=h then List.rev_append l1 t
				else aux x t (h::l1)
	in aux x l [];;


(*let rec compress = function
	| h1::h2::t -> if h1 = h2 then compress (h2::t)
				   else h1 :: compress (h2::t)
	| l -> l;; *)

let compress l =
	let rec aux l1 l = match l with 
		h1::h2::t -> if h1=h2 then aux l1 (h2::t)
					else aux (h1::l1) (h2::t)
		|[h] -> List.rev (h::l1)
		|[] -> List.rev l1
	in aux [] l;;
					
					
					
					
					
					
					
					
					
					
					
					
					
					
		  
