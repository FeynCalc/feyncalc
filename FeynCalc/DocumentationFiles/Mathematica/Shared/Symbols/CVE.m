(* ::Package:: *)

(* ::Section:: *)
(*CVE*)


(* ::Text:: *)
(*`CVE[p, i]` is a $D-4$-dimensional Cartesian vector and is transformed into `CartesianPair[CartesianMomentum[p,D-4], CartesianIndex[i,D-4]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FVE](FVE), [Pair](Pair), [CartesianPair](CartesianPair).*)


(* ::Subsection:: *)
(*Examples*)


CVE[p,i]


CVE[p-q,i]


FCI[CVE[p,i]]//StandardForm


(* ::Text:: *)
(*`ExpandScalarProduct` is used to expand momenta in `CVE`*)


ExpandScalarProduct[CVE[p-q,i]]
