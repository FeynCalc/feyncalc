(* ::Package:: *)

 


(* ::Section:: *)
(*MT*)


(* ::Text:: *)
(*`MT[mu, nu]` is the metric tensor in $4$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCE](FCE.md), [FCI](FCI.md), [MTD](MTD.md), [MTE](MTE.md).*)


(* ::Subsection:: *)
(*Examples*)


MT[\[Alpha],\[Beta]]


Contract[MT[\[Alpha],\[Beta]] MT[\[Alpha],\[Beta]]]


MT[a,b]//StandardForm


FCI[MT[a,b]]//StandardForm


FCE[FCI[MT[a,b]]]//StandardForm
