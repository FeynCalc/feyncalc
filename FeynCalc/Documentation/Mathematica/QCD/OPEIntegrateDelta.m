(* ::Package:: *)

 


(* ::Section:: *)
(*OPEIntegrateDelta*)


(* ::Text:: *)
(*`OPEIntegrateDelta[expr, x, m]` introduces the $\delta(1-x)$ (`DeltaFunction[1-x]`).*)


(* ::Text:: *)
(*The Mathematica `Integrate` function is called and each integration  (from $0$ to $1$) is recorded for reference (and bug-checking) in the list `$MIntegrate`.*)


(* ::Text:: *)
(*Notice that the dimension specified by the option should also be the dimension used in `expr`. It is replaced in OPEIntegrateDelta by (`4+Epsilon`).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Examples*)
