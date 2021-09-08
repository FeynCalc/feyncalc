(* ::Package:: *)

 


(* ::Section:: *)
(*FCGetDimensions*)


(* ::Text:: *)
(*`FCGetDimensions[expr]` is an auxiliary function that determines the dimensions in which 4-momenta and Dirac matrices of the given expression are defined. The result is returned as a list, e.g. `{4}`, `{D}` or `{4,D,D-4}` etc.*)


(* ::Text:: *)
(*This is useful if one wants to be sure that all quantities inside a particular expression are purely $4$-dimensional or purely $D$-dimensional.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ChangeDimension](ChangeDimension.md).*)


(* ::Subsection:: *)
(*Examples*)


FCGetDimensions[GA[i]]


FCGetDimensions[GSD[p]]


FCGetDimensions[FVE[q,\[Mu]]GS[p]]
