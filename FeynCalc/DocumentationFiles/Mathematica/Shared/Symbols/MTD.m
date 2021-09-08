(* ::Package:: *)

 


(* ::Section:: *)
(*MTD*)


(* ::Text:: *)
(*`MTD[mu, nu]` is the metric tensor in $D$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCE](FCE.md), [FCI](FCI.md), [MT](MT.md), [MTE](MTE.md).*)


(* ::Subsection:: *)
(*Examples*)


MTD[\[Alpha],\[Beta]]


Contract[MTD[\[Alpha],\[Beta]] MTD[\[Alpha],\[Beta]]]


MTD[\[Alpha],\[Beta]]//StandardForm


FCI[MTD[\[Alpha],\[Beta]]]//StandardForm


FCE[FCI[MTD[\[Mu],\[Nu]]]]//StandardForm
