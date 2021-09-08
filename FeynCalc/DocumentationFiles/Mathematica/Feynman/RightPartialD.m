(* ::Package:: *)

 


(* ::Section:: *)
(*RightPartialD*)


(* ::Text:: *)
(*`RightPartialD[mu]` denotes $\partial _{\mu }$, acting to the right.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftPartialD](LeftPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


RightPartialD[\[Mu]]


RightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Mu]]]
ExpandPartialD[%]
%//StandardForm


RightPartialD[\[Mu]]//StandardForm
