
open Bin_tree;;

let rec insert_tree ord x = function 
	Empty -> Node (x, Empty, Empty)
	|Node (r, izq, der) -> if (ord x r) then Node (r, insert_tree ord x izq, der)
    				   else Node (r, izq, insert_tree ord x der);;
    				   				   

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l);;

