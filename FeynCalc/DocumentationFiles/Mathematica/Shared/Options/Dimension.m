(* ::Package:: *)

 


(* ::Section:: *)
(*Dimension*)


(* ::Text:: *)
(*`Dimension` is an option of several functions and denotes the number of space-time dimensions. Possible settings are: `4`, `n`, `d`, `D`, ... , the variable does not matter, but it should have head Symbol.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


Options[ScalarProduct]
ScalarProduct[m,n,Dimension->d]
%//StandardForm
