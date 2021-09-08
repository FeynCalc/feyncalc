(* ::Package:: *)

 


(* ::Section:: *)
(*CGA*)


(* ::Text:: *)
(*`CGA[i]` can be used as input for $\gamma^i$ in 4 dimensions, where `i` is a Cartesian index, and is transformed into `DiracGamma[CartesianIndex[i]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GA](GA.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


CGA[i]


CGA[i,j]-CGA[j,i]


StandardForm[FCI[CGA[i]]]


CGA[i,j,k,l]


StandardForm[CGA[i,j,k,l]]


DiracSimplify[DiracTrace[CGA[i,j,k,l]]]


CGA[i] . (CGS[p]+m) . CGA[j]
