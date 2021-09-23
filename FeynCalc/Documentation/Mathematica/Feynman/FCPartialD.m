(* ::Package:: *)

 


(* ::Section:: *)
(*FCPartialD*)


(* ::Text:: *)
(*`FCPartialD[\[Mu]]` denotes the four-dimensional $\partial _{\mu }$.*)


(* ::Text:: *)
(*`FCPartialD` is used to denote derivative fields.*)


(* ::Text:: *)
(*`FCPartialD[LorentzIndex[\[Mu] ,D]]` denotes the $D$-dimensional $\partial _{\mu }$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,{\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%]
StandardForm[%]
