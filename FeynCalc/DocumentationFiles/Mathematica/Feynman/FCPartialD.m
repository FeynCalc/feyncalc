(* ::Package:: *)

 


(* ::Section:: *)
(* FCPartialD *)


(* ::Text:: *)
(*`FCPartialD[\[Mu]]` denotes the four-dimensional $\partial _{\mu }$.*)


(* ::Text:: *)
(*`FCPartialD` is used to denote derivative fields.*)


(* ::Text:: *)
(*`FCPartialD[LorentzIndex[\[Mu] ,D]]` denotes the $D$-dimensional $\partial _{\mu }$.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*ExpandPartialD, LeftPartialD, LeftRightPartialD, RightPartialD.*)


(* ::Subsection:: *)
(* Examples *)


QuantumField[A,{\[Mu]}] . LeftPartialD[\[Nu]]
ExpandPartialD[%]
StandardForm[%]
