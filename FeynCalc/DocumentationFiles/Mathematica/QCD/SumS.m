(* ::Package:: *)

 


(* ::Section:: *)
(*SumS*)


(* ::Text:: *)
(*`SumS[1, m]` is the harmonic number $S_ 1(m) = \sum _ {i=1}^m i^{-1}$.*)


(* ::Text:: *)
(*`SumS[1,1,m]` is $\sum_{i=1}^m S_ 1 (i)/i$.*)


(* ::Text:: *)
(*`SumS[k,l,m]` is $\sum _ {i=1}^m S_l (i)/i^k$.*)


(* ::Text:: *)
(*`SumS[r, n]` represents `Sum[Sign[r]^i/i^Abs[r], {i, 1, n}]`.*)


(* ::Text:: *)
(*`SumS[r,s, n]` is `Sum[Sign[r]^k/k^Abs[r] Sign[s]^j/j^Abs[s], {k, 1, n}, {j, 1, k}]` etc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SumP](SumP.md), [SumT](SumT.md).*)


(* ::Subsection:: *)
(*Examples*)


SumS[1,m-1]


SumS[2,m-1]


SumS[-1,m]


SumS[1,m,Reduce->True]


SumS[3,m+2,Reduce->True]


SetOptions[SumS,Reduce->True];
SumS[3,m+2]


SetOptions[SumS,Reduce->False];
SumS[1,4]


SumS[1,2,m-1]


SumS[1,1,1,11]


SumS[-1,4]


SumT[1,4]
