(* ::Package:: *)

 


(* ::Section:: *)
(*LeftRightPartialD2*)


(* ::Text:: *)
(*`LeftRightPartialD2[\[Mu]]` denotes $\overleftrightarrow{\partial }_{\mu }$, acting to the left and right.*)


(* ::Text:: *)
(*`ExplicitPartialD[LeftRightPartialD2[\[Mu]]] gives `(RightPartialD[\[Mu]] + LeftPartialD[\[Mu]])`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftPartialD](LeftPartialD.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


LeftRightPartialD2[\[Mu]]
ExplicitPartialD[%]


LeftRightPartialD2[\[Mu]] . QuantumField[A,LorentzIndex[\[Nu]]]
ExpandPartialD[%]


QuantumField[A,LorentzIndex[\[Mu]]] . LeftRightPartialD2[\[Nu]] . QuantumField[A,LorentzIndex[\[Rho]]]
ExpandPartialD[%]
