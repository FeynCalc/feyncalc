 
(* ::Section:: *)
(* Complement1 *)
(* ::Text:: *)
(*Complement1[l1, l2] where l1 and l2 are lists returns a list of elements from l1 not in l2. Multiple occurences of an element in l1 are kept and multiple occurences of an element in l2 are dropped multiply if present in l1..*)


(* ::Subsection:: *)
(* Examples *)
Complement[{a,b,c,d,e,f,e},{a,b,c,d}]

Complement1[{a,b,c,d,e,f,e},{a,b,c,d}]
