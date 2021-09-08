(* ::Package:: *)

 


(* ::Section:: *)
(*ExpandPartialD*)


(* ::Text:: *)
(*`ExpandPartialD[exp]` expands noncommutative products of `QuantumField}`'s and partial differentiation operators in `exp` and applies the Leibniz rule.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [PartialDRelations](PartialDRelations.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


RightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]]
ExpandPartialD[%]


LeftRightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Nu]]]
ExpandPartialD[%]


QuantumField[A,LorentzIndex[\[Mu]]] . (LeftRightPartialD[OPEDelta]^2) . QuantumField[A,LorentzIndex[\[Rho]]]
ExpandPartialD[%]


8 LeftRightPartialD[OPEDelta]^3


ExplicitPartialD[%]


ExpandPartialD[%]


LC[\[Mu],\[Nu],\[Rho],\[Tau]] RightPartialD[\[Alpha],\[Mu],\[Beta],\[Nu]]


ExpandPartialD[%]
