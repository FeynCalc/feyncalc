 
(* ::Section:: *)
(*HypExplicit*)
(* ::Text:: *)
(*`HypExplicit[exp, nu]` expresses Hypergeometric functions in exp by their definition in terms of a sum (the `Sum` is omitted and `nu` is the summation index).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [HypergeometricIR](HypergeometricIR.md).*)



(* ::Subsection:: *)
(*Examples*)


Hypergeometric2F1[a,b,c,z]
HypExplicit[%,\[Nu]]


HypergeometricPFQ[{a,b,c},{d,e},z]
HypExplicit[%,\[Nu]]
