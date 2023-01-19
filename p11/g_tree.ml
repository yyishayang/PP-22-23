
type 'a g_tree =
  Gt of 'a * 'a g_tree list;;

(*val size : 'a g_tree -> int
 devuelve el número de nodos de un g_tree *)
let rec size = function 
    Gt (_, []) -> 1
  | Gt (r, h::t) -> size h + size (Gt (r, t));;


(*val height : 'a g_tree -> int
 devuelve la "altura", como número de niveles, de un g_tree *)
 
let rec height = 
	let rec aux maxi l = match l with 
		[] -> maxi 
		|h::t -> let m = height h in 
				 if m >= maxi then aux m t
				 else aux maxi t
	in function 
	 Gt (_, []) -> 1
	|Gt(_, l) -> 1 + aux 0 l;;
	
	
(*val leaves : 'a g_tree -> 'a list
 devuelve las hojas de un g_tree, "de izquierda a derecha" *)

let rec leaves = function
	Gt (r, []) -> [r]
	|Gt (r, l) -> List.concat (List.map leaves l);;


(*val mirror : 'a g_tree -> 'a g_tree
 devuelve la imagen especular de un g_tree *)

let rec mirror = function 
	Gt (r, l) -> Gt (r, List.map mirror (List.rev l));;


(*val preorder : 'a g_tree -> 'a list
 devuelve la lista de nodos de un g_tree en "preorden" *)

let rec preorder = function 
	Gt (r, []) -> [r]
	|Gt (r, l) -> r::List.concat (List.map preorder l);;


(*val postorder : 'a g_tree -> 'a list
 devuelve la lista de nodos de un g_tree en "postorden" *)
 let rec postorder = function
 	Gt (r, []) -> [r]
	|Gt (r, l) -> List.concat (List.map postorder l) @ [r];;
	
	
let rec lmax l maxi = match l with 
  [] -> maxi  
  |h::t -> if h>maxi then lmax t h 
  			else lmax t maxi;;

	
	
	
 
