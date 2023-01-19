let rec qsort1 ord = function
	[] -> []
	| h::t -> let after, before = List.partition (ord h) t in
			  qsort1 ord before @ h :: qsort1 ord after;;
			  
     
(* Esta implementación no tiene buen rendimiento cuando tuviese que ordenar una lista ordenada 
de un gran número de elementos porque para comprobar que está ordenada divide a la mitad la lista 
consecutivamente cuando podría implementarse de tal forma que sólo comprobase que estuviese ordenada *)
		  
		  
let rec qsort2 ord =
	let append' l1 l2 = List.rev_append (List.rev l1) l2 in
	function
		[] -> []
		| h::t -> let after, before = List.partition (ord h) t in
				  append' (qsort2 ord before) (h :: qsort2 ord after);;
				  

(* La ventaja principal de qsort2 sobre qsort1 es que puede ordenar listas de mayor longitud, por lo 
que no habrá la posibilidad de que diese stack_overflow. Por ejemplo:*)

let l1 = List.init 1_000_000 (function x -> Random.int 1_000_000);;
(*De modo que qsort1 daría stack_overflow y qsort2 ordenaría la lista. *)


(*La desventaja que tiene qsort2 sobre qsort1 es que es más lenta debido a que para paliar la no 
terminalidad del @, se usa la funcion rev_append después de rev realizando más operaciones e incrementando
el tiempo que se tarda en ejecutar*)  


(*let crono f x = 
      let t = Sys.time () in
      f x; Sys.time () -. t;;
      
let l = List.init 40000 (function x -> Random.int 40000);;
crono (qsort2 (<=)) l;;
crono (qsort1 (<=)) l;;

El tiempo de ejecución que tarda qsort1 y qsort2 para una lista de 40000 elementos es 0.0530229999999999313 y 0.0696320000000000272, respectivamente. 
Por tanto, la penalización es de un 31%
*)


