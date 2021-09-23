(* ::Package:: *)

 


(* ::Section:: *)
(*CGAE*)


(* ::Text:: *)
(*`CGAE[i]` can be used as input for $\gamma ^i$ in $D-4$ dimensions, where `i` is a Cartesian index, and is transformed into `DiracGamma[CartesianIndex[i,D-4],D-4]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GAE](GAE.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


CGAE[i]


CGAE[i,j]-CGAE[j,i]


StandardForm[FCI[CGAE[i]]]


CGAE[i,j,k,l]


StandardForm[CGAE[i,j,k,l]]
