false && (2 / 0 > 0);; (* -: bool = false *)

true && (2 / 0 > 0);; (* error de ejecución puesto que al evaluar la segunda componente hay una división entre 0*)

true || (2 / 0 > 0);; (*-: bool = true *)

false || (2 / 0 > 0);; (* error de ejecución puesto que al evaluar la segunda condición hay una división entre 0*)

let con b1 b2 = b1 && b2;; (* val con: bool -> bool -> bool = <fun>*)

let dis b1 b2 = b1 || b2;; (* val dis: bool -> bool -> bool = <fun>*)

con (1 < 0) (2 / 0 > 0);; (*teniendo en cuenta que "con" es una función, se va a evaluar primero los dos parámetros recibidos. Por tanto, como encuentra que uno de ellos es una división por 0, se produce un error de ejecución*)

(1 < 0) && (2 / 0 > 0);; (* -: bool = false*)

dis (1 > 0) (2 / 0 > 0);; (*teniendo en cuenta que "dis" es una función, se va a evaluar primero los dos parámetros recibidos. Por tanto, como encuentra que uno de ellos es una división por 0, se produce un error de ejecución*)

(1 > 0) || (2 / 0 > 0);; (*-: bool = true*)
