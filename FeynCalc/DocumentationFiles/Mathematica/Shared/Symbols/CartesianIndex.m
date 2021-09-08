(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianIndex*)


(* ::Text:: *)
(*`CartesianIndex` is the head of Cartesian indices. The internal representation of a $3$-dimensional `i` is `CartesianIndex[i]`.*)


(* ::Text:: *)
(*For other than three dimensions: `CartesianIndex[i, Dimension]`.*)


(* ::Text:: *)
(*`CartesianIndex[i, 3]` simplifies to `CartesianIndex[i]`. The first argument cannot be an integer.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LorentzIndex](LorentzIndex.md), [ExplicitLorentzIndex](ExplicitLorentzIndex.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This denotes a 3-dimensional Cartesian index.*)


CartesianIndex[i]


(* ::Text:: *)
(*An optional second argument can be given for a dimension different from 3.*)


CartesianIndex[i,D-1]


CartesianIndex[i,D-4]
