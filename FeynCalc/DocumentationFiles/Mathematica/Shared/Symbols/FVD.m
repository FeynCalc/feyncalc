(* ::Package:: *)

 


(* ::Section:: *)
(*FVD*)


(* ::Text:: *)
(*`FVD[p, mu]` is the $D$-dimensional vector $p$ with Lorentz index `mu`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCE](FCE.md), [FCI](FCI.md), [FV](FV.md), [Pair](Pair.md).*)


(* ::Subsection:: *)
(*Examples*)


FVD[p,\[Mu]]


FVD[p-q,\[Mu]]


FVD[p,\[Mu]]//StandardForm


FCI[FVD[p,\[Mu]]]//StandardForm


(* ::Text:: *)
(*There is no special function to expand momenta in `FVD`.*)


ExpandScalarProduct[FVD[p-q,\[Mu]]]
StandardForm[%]
