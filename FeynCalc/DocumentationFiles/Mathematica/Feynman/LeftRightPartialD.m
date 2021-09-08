(* ::Package:: *)

 


(* ::Section:: *)
(*LeftRightPartialD*)


(* ::Text:: *)
(*`LeftRightPartialD[mu]` denotes $\overleftrightarrow {\partial }_{\mu }$, acting to the left and right.*)


(* ::Text:: *)
(*`ExplicitPartialD[LeftRightPartialD[\[Mu]]]` gives `1/2 (RightPartialD[\[Mu]] - LeftPartialD[\[Mu]])`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD2](LeftRightPartialD2.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


LeftRightPartialD[\[Mu]]
ExplicitPartialD[%]


LeftRightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Nu]]]
ExpandPartialD[%]


QuantumField[A,LorentzIndex[\[Mu]]] . LeftRightPartialD[\[Nu]] . QuantumField[A,LorentzIndex[\[Rho]]]
ExpandPartialD[%]
