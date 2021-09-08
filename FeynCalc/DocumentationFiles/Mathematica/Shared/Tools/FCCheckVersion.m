(* ::Package:: *)

 


(* ::Section:: *)
(*FCCheckVersion*)


(* ::Text:: *)
(*`FCCheckVersion[major, minor, build]` checks if the current version of FeynCalc is larger or equal than `marjor.minor.build`. For example, `FCCheckVersion[9,3,0]` will generate a warning (when running with the frontend) or quit kernel (when running without the frontend) if the loaded FeynCalc version is older than 9.3.0.*)


(* ::Text:: *)
(*Notice that this function is available only since FeynCalc 9.3.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [$FeynCalcVersion]($FeynCalcVersion.md).*)


(* ::Subsection:: *)
(*Examples*)


FCCheckVersion[8,2,0]


(*FCCheckVersion[15,2,0]*)
