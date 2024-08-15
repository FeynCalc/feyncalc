(* ::Package:: *)

 


(* ::Section:: *)
(*LeftRightNablaD2*)


(* ::Text:: *)
(*`LeftRightNablaD2[mu]` denotes $\overleftrightarrow{\nabla }_{i}$, acting to the left and right.*)


(* ::Text:: *)
(*`ExplicitPartialD[LeftRightNablaD2[mu]]` gives `(RightNablaD[i] + LeftNablaD[i])`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftNablaD](LeftNablaD.md), [RightNablaD](RightNablaD.md).*)


(* ::Subsection:: *)
(*Examples*)


LeftRightNablaD2[i]

ExplicitPartialD[%]


LeftRightNablaD2[i] . QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]


QuantumField[A,LorentzIndex[\[Mu]]] . LeftRightNablaD2[i] . QuantumField[A,LorentzIndex[\[Rho]]]

ExpandPartialD[%]
