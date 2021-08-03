(* ::Package:: *)

 


(* ::Section:: *)
(*TGA*)


(* ::Text:: *)
(*`TGA[]`  can be used as input for $\gamma^0$ in $4$ dimensions and is transformed into `DiracGamma[ExplicitLorentzIndex[0]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[GA](GA), [DiracGamma](DiracGamma).*)


(* ::Subsection:: *)
(*Examples*)


TGA[]


TGA[]//FCI//StandardForm


TGA[] . TGA[]//DiracSimplify
