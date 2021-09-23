(* ::Package:: *)

 


(* ::Section:: *)
(*SumP*)


(* ::Text:: *)
(*`SumP[k, m]` is $2^{k-1}\sum _{i=1}^{2m}\left(1+(-1)^i\right)/i^k$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SumS](SumS.md), [SumT](SumT.md).*)


(* ::Subsection:: *)
(*Examples*)


SumP[1,m-1]


SumP[2,m-1]


SumP[1,m]


SumP[1,4]


Explicit[SumP[1,n/2]]
%/.n->8
