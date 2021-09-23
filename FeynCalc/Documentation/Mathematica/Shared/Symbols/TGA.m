(* ::Package:: *)

 


(* ::Section:: *)
(*TGA*)


(* ::Text:: *)
(*`TGA[]`  can be used as input for $\gamma^0$ in $4$ dimensions and is transformed into `DiracGamma[ExplicitLorentzIndex[0]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GA](GA.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


TGA[]


TGA[]//FCI//StandardForm


TGA[] . TGA[]//DiracSimplify
