(* ::Package:: *)

 


(* ::Section:: *)
(*PartialDRelations*)


(* ::Text:: *)
(*`PartialDRelations` is an option for `ExpandPartialD`. It is a list of rules applied by `ExpandPartialD` at the end.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCPartialD](FCPartialD), [ExpandPartialD](ExpandPartialD), [LeftPartialD](LeftPartialD), [LeftRightPartialD](LeftRightPartialD), [RightPartialD](RightPartialD).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,{\[Mu]}] . QuantumField[B,{\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%, PartialDRelations->{A->C}]
