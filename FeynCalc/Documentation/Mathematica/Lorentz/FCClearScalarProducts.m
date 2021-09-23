(* ::Package:: *)

 


(* ::Section:: *)
(*FCClearScalarProducts*)


(* ::Text:: *)
(*`FCClearScalarProducts[]` removes all user-performed specific settings for ScalarProduct's.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPD](SPD.md).*)


(* ::Subsection:: *)
(*Examples*)


ScalarProduct[p,p]=m^2;
Pair[Momentum[p],Momentum[p]]


FCClearScalarProducts[]
Pair[Momentum[p],Momentum[p]]
SP[p,p]



