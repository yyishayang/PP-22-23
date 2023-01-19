let rec divide l = match l with
	h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
	| _ -> l, [];;
	
	
let rec merge = function
	[], l | l, [] -> l
	| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
						else h2 :: merge (h1::t1, t2);;

				
let rec msort1 l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide l in
			merge (msort1 l1, msort1 l2);;

(*Debido a la no terminalidad de divide y merge, puede dar stack_overflow cuando una lista tiene un 
gran número de elementos.*)
let l2 = List.init 1_000_000 (function x -> Random.int 1_000_000);;

(*--------------------------------------------------------------------------------------------------*)
	
let divide' l = 
	let rec aux l t1 t2= match l with 
		[] -> List.rev t1, List.rev t2
		|[h] -> List.rev (h::t1), t2
		|h1::h2::t -> aux t (h1::t1) (h2::t2)
	in aux l [] [];;

let merge' f l1 = 
	let rec aux f l1 fin = match l1 with 
      [], l | l, [] -> l
    | h1::t1, h2::t2 -> if f h1 h2 then aux f (t1,h2::t2) (h1::fin) 
    					else aux f (h1::t1,t2) (h2::fin)
    in aux f l1 [];;
					

let rec msort2 f l = match l with
	[] | _::[] -> l
	| l -> let l1, l2 = divide' l in
			merge' f (msort2 f l1, msort2 f l2);;
			
			
(*let crono f x = 
	let t = Sys.time() in
		f x; Sys.time () -. t;;


let l = List.init 10_000 (function x -> Random.int 10_000);;	
El tiempo de ejecución obtuvidos para los algoritmos msort1, msort2 y qsort2 empleando una lista 
aleatoria (l) de 10_000 elementos es 0.008728s, 0.003964s y 0.011244s, respectivamente.


let l1 = List.init 10_000 abs;;
Para una lista con inicialización ascendente de 10_000 elementos (l1), el tiempo de ejecución es 
0.007089s, 0.008674s y 3.027445s.

En conclusión, para una lista aleatoria la mejor opción es msort2 pero cuando la lista está ya ordenada 
msort2 es un poco más lento que msort1. 
*)







