(* let curry = function f -> (function x -> (function y -> f (x,y)));; *)

let curry f x y = 
	f (x,y);; 

(* let uncurry =  function f -> function (x,y) -> (f x) y;; *)
let uncurry f (x,y) =
	(f x) y;;


uncurry (+);; (* -: int * int -> int = <fun>*)
let sum = (uncurry (+));; (* val sum: int * int -> int *)
(*sum 1;; error de tipo puesto que es necesario otro valor de tipo int *)
sum (2,1);; (* -: int = 3 *)
let g = curry (function p -> 2 * fst p + 3 * snd p);; (* val g: int -> int -> int = <fun> *)
(*g (2,5);; error de tipo porque se introdujo una expresión de tipo int*int cuando debería ser de tipo int, una expresión válida sería g 2*)
let h = g 2;; (* val h: int -> int = <fun> *) 
h 1, h 2, h 3;; (* int*int*int = (7,10,13)*)

(* let comp = function f -> function g -> function x -> f (g x);; *)
let comp f g x = 
	f (g x);;

let f = let square x = x * x in comp square ((+) 1);; (* val f: int -> int = <fun> *)
f 1, f 2, f 3;; (* -: int*int*int = (4,9,16) *)

let i x = x;;
let j (x,y) = x;;
let k (x,y) = y;;
let l x = x::[];;

(*Se puede escribir una única función cuando se refiere a como definir la función, por ejemplo que vaya de 'a en 'a.
Pero son infinitas cuando se refiere a cuántas funciones cumplen con el tipo dado pues hay tantas como tipos existen *)
