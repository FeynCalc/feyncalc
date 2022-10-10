(* ::Package:: *)

 


(* ::Section:: *)
(*LeftRightNablaD*)


(* ::Text:: *)
(*`LeftRightNablaD[i]` denotes $\overleftrightarrow {\nabla}_{i}$, acting to the left and right.*)


(* ::Text:: *)
(*`ExplicitPartialD[LeftRightNablaD[i]]` gives `1/2 (RightNablaD[i] - LeftNablaD[i])`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftNablaD](LeftNablaD.md), [LeftRightNablaD2](LeftRightNablaD2.md), [RightNablaD](RightNablaD.md).*)


(* ::Subsection:: *)
(*Examples*)


LeftRightNablaD[i]

ExplicitPartialD[%]


LeftRightNablaD[i] . QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]


QuantumField[A,LorentzIndex[\[Mu]]] . LeftRightNablaD[i] . QuantumField[A,LorentzIndex[\[Rho]]]

ExpandPartialD[%]



