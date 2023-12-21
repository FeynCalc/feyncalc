(* ::Package:: *)

 


(* ::Section:: *)
(*CV*)


(* ::Text:: *)
(*`CV[p, i]` is a 3-dimensional Cartesian vector and is transformed into `CartesianPair[CartesianMomentum[p], CartesianIndex[i]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FV](FV.md), [Pair](Pair.md), [CartesianPair](CartesianPair.md).*)


(* ::Subsection:: *)
(*Examples*)


CV[p,i]


CV[p-q,i]


FCI[CV[p,i]]//StandardForm


(* ::Text:: *)
(*`ExpandScalarProduct` is used to expand momenta in `CV`*)


ExpandScalarProduct[CV[p-q,i]]
