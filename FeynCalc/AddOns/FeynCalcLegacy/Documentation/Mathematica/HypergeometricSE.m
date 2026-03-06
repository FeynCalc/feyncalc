(* ::Package:: *)

 


(* ::Section:: *)
(*HypergeometricSE*)


(* ::Text:: *)
(*`HypergeometricSE[exp, nu]` expresses Hypergeometric functions by their series expansion in terms of a sum (the `Sum` is omitted and `nu`, running from $0$ to $\infty$, is the summation index).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [HypergeometricIR](HypergeometricIR.md).*)


(* ::Subsection:: *)
(*Examples*)


HypergeometricSE[Hypergeometric2F1[a,b,c,z],\[Nu]]


HypergeometricSE[HypergeometricPFQ[{a,b,c},{d,e},z],\[Nu]]
