 
(* ::Section:: *)
(* Factor2 *)
(* ::Text:: *)
(*Factor2[poly] factors a polynomial in a standard way. Factor2 works sometimes better than Factor on polynomials involving rationals with sums in the denominator. Factor2 uses Factor internally and is in general slower than Factor. There are four possible settings of the option Method (0,1,2,3). In general Factor will work faster than Factor2..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Collect2.*)



(* ::Subsection:: *)
(* Examples *)



(a-x)(b-x)

{Factor2[%], Factor[%]}

Expand[(a-b)(a+b)]

Factor[%]

Factor2[%%]

Factor2[%%%,FactorFull->True]
