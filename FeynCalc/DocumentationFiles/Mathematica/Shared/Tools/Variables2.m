 
(* ::Section:: *)
(* Variables2 *)
(* ::Text:: *)
(*Variables2[expr] is like Variables, but it also works on rules and equalities as well as lists thereof. Variables2 always applies Union to the output..*)


(* ::Subsection:: *)
(* Examples *)
Variables[{a->x1+y1,b->x2+y2}]

Variables2[{a->x1+y1,b->x2+y2}]

Variables[a+b==c+d]

Variables2[a+b==c+d]
