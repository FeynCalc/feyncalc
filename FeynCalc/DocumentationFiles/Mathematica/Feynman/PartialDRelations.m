(* ::Package:: *)

 


(* ::Section:: *)
(*PartialDRelations*)


(* ::Text:: *)
(*`PartialDRelations` is an option for `ExpandPartialD`. It is a list of rules applied by `ExpandPartialD` at the end.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCPartialD](FCPartialD.md), [ExpandPartialD](ExpandPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,{\[Mu]}] . QuantumField[B,{\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%, PartialDRelations->{A->C}]
