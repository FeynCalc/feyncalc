(* ::Package:: *)

 


(* ::Section:: *)
(*KD*)


(* ::Text:: *)
(*`KD[i, j]`  is the Kronecker delta in $3$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[CartesianPair](CartesianPair), [KDD](KDD).*)


(* ::Subsection:: *)
(*Examples*)


KD[i,j]


Contract[KD[i,j] KD[i,j]]


KD[a,b]//StandardForm


FCI[KD[a,b]]//StandardForm


FCE[FCI[KD[a,b]]]//StandardForm
