(* ::Package:: *)

 


(* ::Section:: *)
(*FCPrepareFAAmp*)


(* ::Text:: *)
(*`FCPrepareFAAmp[exp]`  is an auxiliary function for a partial conversion of a FeynArts amplitude to FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[]
FeynArts`FAFeynAmpDenominator[FeynArts`FAPropagatorDenominator[Momentum[P,D],MW Sqrt[FeynArts`FAGaugeXi[W]]],FeynArts`FAPropagatorDenominator[Momentum[k,D],m]]
FCPrepareFAAmp[%]


FeynArts`IndexDelta[FeynArts`Index[Global`Gluon,1],FeynArts`Index[Global`Gluon,2]]
FCPrepareFAAmp[%]
