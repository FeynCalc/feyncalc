(* ::Package:: *)

 


(* ::Section:: *)
(*MetricTensor*)


(* ::Text:: *)
(*`MetricTensor[mu, nu]` is the metric tensor. The default dimension is $4$.*)


(* ::Text:: *)
(*The shortcut `MetricTensor` is deprecated, please use `MT` instead!*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCI](FCI.md), [MT](MT.md), [MTD](MTD.md).*)


(* ::Subsection:: *)
(*Examples*)


MetricTensor[\[Alpha],\[Beta]]
Contract[% %]


MetricTensor[\[Alpha],\[Beta],Dimension->D]
Contract[% %]


StandardForm[MetricTensor[a,b]]


(* ::Text:: *)
(*`MetricTensor` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `MT`.*)


MT[\[Mu],\[Nu]]


MTD[\[Mu],\[Nu]]


FCI[MT[\[Mu],\[Nu]]]===MetricTensor[\[Mu],\[Nu]]
FCI[MTD[\[Mu],\[Nu]]]===MetricTensor[\[Mu],\[Nu],Dimension->D]



