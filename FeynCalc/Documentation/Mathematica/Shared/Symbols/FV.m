(* ::Package:: *)

 


(* ::Section:: *)
(*FV*)


(* ::Text:: *)
(*`FV[p, mu]` is the $4$-dimensional vector $p^{\mu }$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCE](FCE.md), [FCI](FCI.md), [FVD](FVD.md), [Pair](Pair.md).*)


(* ::Subsection:: *)
(*Examples*)


FV[p,\[Mu]]


FV[p-q,\[Mu]]


FV[p,\[Mu]]//StandardForm


FCI[FV[p,\[Mu]]]//StandardForm


(* ::Text:: *)
(*`ExpandScalarProduct` is used to expand momenta in `FV`*)


ExpandScalarProduct[FV[p-q,\[Mu]]]
