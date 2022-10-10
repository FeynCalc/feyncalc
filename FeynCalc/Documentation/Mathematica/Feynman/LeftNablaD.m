(* ::Package:: *)

 


(* ::Section:: *)
(*LeftNablaD*)


(* ::Text:: *)
(*`LeftNablaD[i]` denotes $\overleftarrow{\nabla}_{i}$ acting to the left.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftRightNablaD](LeftRightNablaD.md), [RightNablaD](RightNablaD.md).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,LorentzIndex[\[Mu]]] . LeftNablaD[i]

ex=ExpandPartialD[%]


ex//StandardForm


StandardForm[LeftNablaD[i]]


QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]] . LeftNablaD[i]

ex=ExpandPartialD[%]


ex//StandardForm
