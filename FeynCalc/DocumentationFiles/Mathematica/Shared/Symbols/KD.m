(* ::Package:: *)

 


(* ::Section:: *)
(*KD*)


(* ::Text:: *)
(*`KD[i, j]`  is the Kronecker delta in $3$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [KDD](KDD.md).*)


(* ::Subsection:: *)
(*Examples*)


KD[i,j]


Contract[KD[i,j] KD[i,j]]


KD[a,b]//StandardForm


FCI[KD[a,b]]//StandardForm


FCE[FCI[KD[a,b]]]//StandardForm
