(* ::Package:: *)

 


(* ::Section:: *)
(*LorentzIndex*)


(* ::Text:: *)
(*`LorentzIndex[mu]` denotes a $4$-dimensional Lorentz index.*)


(* ::Text:: *)
(*For other than $4$ dimensions: `LorentzIndex[mu, D]` or `LorentzIndex[mu]` etc.*)


(* ::Text:: *)
(*` LorentzIndex[mu, 4]` simplifies to `LorentzIndex[mu]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ChangeDimension](ChangeDimension.md), [Momentum](Momentum.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This denotes a $4$-dimensional Lorentz index.*)


LorentzIndex[\[Alpha]]


(* ::Text:: *)
(*An optional second argument can be given for a dimension different from $4$.*)


LorentzIndex[\[Alpha],n]
