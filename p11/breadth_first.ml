
open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

	
let breadth_first_t gt = 
    let rec aux l next_level current_level = match current_level with 
        [] -> if next_level = [] then l 
            else aux l [] (List.rev next_level) 
        | Gt (x, lx)::t -> aux (x::l) (List.rev_append lx next_level) t  
    in aux [] [] [gt];;	


let h = function x -> Gt (x,[]);;
let l = List.init 10_000_000 h;;
let t2 = Gt (1, l) ;;
