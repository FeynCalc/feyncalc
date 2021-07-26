(* ::Package:: *)

(* ::Section:: *)
(*MT*)


(* ::Text:: *)
(*`MT[mu, nu]` is the metric tensor in $4$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FeynCalcExternal](FeynCalcExternal), [FCE](FCE), [FCI](FCI), [MTD](MTD), [MTE](MTE).*)


(* ::Subsection:: *)
(*Examples*)


MT[\[Alpha],\[Beta]]


Contract[MT[\[Alpha],\[Beta]] MT[\[Alpha],\[Beta]]]


MT[a,b]//StandardForm


FCI[MT[a,b]]//StandardForm


FCE[FCI[MT[a,b]]]//StandardForm
