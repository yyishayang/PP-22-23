let hd l = match l with
	[] -> raise (Failure "hd")
	| h::_ -> h;;
	
let tl l = match l with 
	[] -> raise (Failure "tl")
	| _::[] -> []
	| _::t -> t;;
	
	
let length l =
	let rec aux_length i l = match l with 
		[] -> i
		| _::t -> aux_length (i+1) t
	in aux_length 0 l;;
	
	
let rec compare_lengths l1 l2 = match l1, l2 with
	[], [] -> 0
	| [], _ -> -1
	| _, [] -> 1
	| h1::t1, h2::t2 -> compare_lengths t1 t2;;
	

let rec nth l n = 
	if n<0 then raise (Invalid_argument "nth")
	else match l, n with 
		[], _ -> raise (Failure "nth")
		| h::_, 0 -> h
		| _::t, _ -> nth t (n-1);;


let rec append l1 l2 = match l1 with 
	[]-> l2
	|h1::t1 -> h1:: append t1 l2;;
	

let rec find f l = match l with 
	[] -> raise Not_found
	|h::t -> if f h then h
			 else find f t;;


let rec for_all f l = match l with 
	[] -> true
	|h::t -> (f h) && (for_all f t);;
			 
		 
let rec exists f l = match l with 
	[] -> false
	|h::t -> (f h) || (exists f t);;
	


let rec mem x l = match l with 
	 [] -> false
	 |h::t -> if x=h then true 
	 		  else mem x t;;
	 
		 
let filter f l = 
	let rec aux_filter f l l' = match l with 
		[] -> List.rev l'
		|h::t -> if f h then aux_filter f t (h::l')
				else aux_filter f t l'
	in aux_filter f l [];;
		 

let find_all f l = filter f l ;;


let partition f l = 
	let rec aux l x y = match l with 
		[] -> x, y
		|h::t -> if (f h) then aux t (h::x) y
				 else aux t x (h::y)
	in aux l [] [];;
		
	
let rec split l = match l with 
	[] -> [],[]
	|(h1,h2)::t -> let t1,t2 = 
				   split t in (h1::t1), (h2::t2);;
	
	
let rec combine l1 l2 = match l1,l2 with 
	[], [] -> []
	|[], _::_ -> raise (Invalid_argument "combine")
	|_::_, [] -> raise (Invalid_argument "combine")
	|h1::t1, h2::t2 -> (h1,h2):: (combine t1 t2);;

let rev l = 
	let rec aux l lrev = match l with 
		[] -> lrev
		|h::t -> aux t (h::lrev)
	in aux l [];;
	

let init len f =
	if len<0 then raise (Invalid_argument "init")
	else let rec aux cnt l =
				if len=cnt then rev l
				else aux (cnt+1) ((f cnt)::l)
		 in aux 0 [];;
	
	
let rec rev_append l1 l2= match l1 with 
	[] -> l2
	|h::t -> rev_append t (h::l2);;


let rec concat l = match l with 
	[]->[]
	|h::t-> append h (concat t);;             

let flatten l = concat l;;
 

let rec map f l = match l with 
	[] -> []
	|h::t -> f h :: map f t;;


let map f l = 
	let rec aux l' = match l with 
		[] -> l'
		|h::t -> aux ((f h) :: l')
	in aux [];;
	
	
let rec rev_map f l = rev (map f l);;


let rec map2 f l1 l2 = match l1, l2 with 
		[],[] -> []
		|[], _::_ -> raise (Invalid_argument "map2")
		|_::_, [] -> raise (Invalid_argument "map2")
		|h1::t1, h2::t2 -> (f h1 h2) :: (map2 f t1 t2);;


let rec fold_left f a l = match l with 
	[] -> a
	|h::t -> fold_left f (f a h) t;;
 
 
let rec fold_right f l b= match l with  
	[] -> b
	|h::t -> (f h) (fold_right f t b);;
	
	

		

