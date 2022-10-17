let rec power x y = 
	if y= 0 then 1
	else x*power x (y-1);;

let rec power' x y = 
	if y = 0 then 1
	else if y mod 2 = 0
			then power' (x*x) (y/2)
		 else x* power' (x*x) (y/2);;
(* power' es mucho más eficiente debido a que se decrementa a la mitad el número de llamadas recursivas a la función, pero la eficiencia no será visible cuando los valores sean muy pequeños*)

		 
let rec powerf x n = 
	if n = 0 then 1.
	else if n mod 2 = 0
			then powerf (x *. x) (n/2)
		 else x *. powerf (x *. x) (n/2);;
