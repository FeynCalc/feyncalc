(* ::Package:: *)

 


(* ::Section:: *)
(*SumT*)


(* ::Text:: *)
(*`SumT[1, m]` is the alternative harmonic number $\sum _{i=1}^m (-1){}^{\wedge}i/i$ *)


(* ::Text:: *)
(*`SumT[r, n]` represents `Sum[(-1)^i/i^r, {i,1,n}]`*)


(* ::Text:: *)
(*`SumT[r,s, n]` is `Sum[1/k^r (-1)^j/j^s, {k, 1, n}, {j, 1, k}]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SumP](SumP.md), [SumS](SumS.md).*)


(* ::Subsection:: *)
(*Examples*)


SumT[1,m-1]


SumT[2,m-1]


SumT[1,m]


SumT[1,m,Reduce->True]


SumT[1,4]


SumT[1,2,m-1]


SumT[1,2,42]


SumT[1,4]


SumS[-1,4]


SumT[1,2,12]


SumS[1,-2,42]


Array[SumT,6]


Array[SumS[-2,1,#1]&,6]
