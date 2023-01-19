let rec remove x l = match l with 
	[] -> []
	|h::t -> if h=x then t 
			else h:: remove x t;;
			
let rec remove_all x l = match l with 
	[] -> []
	|h::t -> if h=x then remove_all x t
			else h:: remove_all x t;;
			
let rec ldif l1 l2 = match l1,l2 with 
	[], [] -> []
	|_, [] -> l1
	|[], _ -> []	
	|h1::t1, h2::t2 -> ldif (remove_all h2 l1) t2;;


let lprod l1 l2 = 
	let rec aux_lprod l l1 l2 = match l1,l2 with 
		[], _ -> List.rev l
		|h::t, [] -> aux_lprod l t l2
		|h1::t1, h2::t2 -> aux_lprod ((h1,h2)::l) l1 t2
	in aux_lprod [] l1 l2;;


let rec divide l = match l with 
	h1::h2::t -> let t1,t2 = divide t in 
				  h1::t1, h2::t2
	|l-> l,[];;

