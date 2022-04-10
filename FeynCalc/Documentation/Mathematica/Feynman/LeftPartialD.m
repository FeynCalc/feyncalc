(* ::Package:: *)

 


(* ::Section:: *)
(*LeftPartialD*)


(* ::Text:: *)
(*`LeftPartialD[mu]` denotes $\overleftarrow{\partial }_{\mu }$ acting to the left.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,LorentzIndex[\[Mu]]] . LeftPartialD[\[Nu]]

ex=ExpandPartialD[%]


ex//StandardForm


StandardForm[LeftPartialD[\[Mu]]]


QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]] . LeftPartialD[\[Rho]]

ex=ExpandPartialD[%]


ex//StandardForm
