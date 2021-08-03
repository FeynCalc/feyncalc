(* ::Package:: *)

 


(* ::Section:: *)
(*KDD*)


(* ::Text:: *)
(*`KDD[i, j]` is the Kronecker delta in $D-1$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[CartesianPair](CartesianPair), [KD](KD).*)


(* ::Subsection:: *)
(*Examples*)


KDD[i,j]


Contract[KDD[i,j] KDD[i,j]]


KDD[a,b]//StandardForm


FCI[KDD[a,b]]//StandardForm


FCE[FCI[KDD[a,b]]]//StandardForm
