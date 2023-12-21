(* ::Package:: *)

 


(* ::Section:: *)
(*RightNablaD*)


(* ::Text:: *)
(*`RightNablaD[i]` denotes $\nabla _{i}$, acting to the right.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [FCPartialD](FCPartialD.md), [LeftNabalD](LeftNablaD.md).*)


(* ::Subsection:: *)
(*Examples*)


RightNablaD[i]


RightNablaD[i] . QuantumField[A,LorentzIndex[\[Mu]]]

ex=ExpandPartialD[%]


ex//StandardForm


RightNablaD[i]//StandardForm
