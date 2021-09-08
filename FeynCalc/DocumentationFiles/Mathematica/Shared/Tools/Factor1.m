(* ::Package:: *)

 


(* ::Section:: *)
(*Factor1*)


(* ::Text:: *)
(*`Factor1[poly]` factorizes common terms  in the summands of poly. It uses basically `PolynomialGCD`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).*)


(* ::Subsection:: *)
(*Examples*)


(a-x)(b-x)
{Factor1[%], Factor[%]}


ex=Expand[(a-b)(a+b)]


Factor[ex]


Factor1[ex]
