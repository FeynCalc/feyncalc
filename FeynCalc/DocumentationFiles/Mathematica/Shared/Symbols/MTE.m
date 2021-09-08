(* ::Package:: *)

 


(* ::Section:: *)
(*MTE*)


(* ::Text:: *)
(*`MTE[mu, nu]` is the metric tensor in $D-4$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCE](FCE.md), [FCI](FCI.md), [MT](MT.md), [MTD](MTD.md).*)


(* ::Subsection:: *)
(*Examples*)


MTE[\[Alpha],\[Beta]]


Contract[MTE[\[Alpha],\[Beta]] MTE[\[Alpha],\[Beta]]]


Contract[MTE[\[Alpha],\[Beta]] MT[\[Alpha],\[Beta]]]


Contract[MTE[\[Alpha],\[Beta]] MTD[\[Alpha],\[Beta]]]


MTE[\[Alpha],\[Beta]]//StandardForm


MTE[\[Alpha],\[Beta]]


FCI[MTE[\[Alpha],\[Beta]]]//StandardForm


FCE[FCI[MTE[\[Mu],\[Nu]]]]//StandardForm


MTE[\[Mu],\[Nu]]
