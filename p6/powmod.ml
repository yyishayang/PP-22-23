(*
let rec power' x y =
	if y = 0 then 1
	else if (y mod 2) = 0 then power' (x*x) (y/2)
		else x * power' (x*x) (y/2)


let powmod m b e = 
	power' b e mod m
*)

let rec powmod m b e =
	if e = 1 then b mod m
	else if e mod 2 = 0 then powmod m (((b mod m)*(b mod m)) mod m) (e/2)
	else ((b mod m) * powmod m (b mod m) (e-1)) mod m;;


