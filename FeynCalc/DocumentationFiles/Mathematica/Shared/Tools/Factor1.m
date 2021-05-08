 
(* ::Section:: *)
(* Factor1 *)
(* ::Text:: *)
(*Factor1[poly] factorizes common terms  in the summands of poly. It uses basically PolynomialGCD..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Factor2.*)



(* ::Subsection:: *)
(* Examples *)



(a-x)(b-x)

{Factor1[%], Factor[%]}

Expand[(a-b)(a+b)]

Factor[%]

Factor1[%%]
