(* ::Package:: *)

 


(* ::Section:: *)
(*DropScaleless*)


(* ::Text:: *)
(*`DropScaleless` is an option for `FCLoopIsolate`, `ApartFF`, `FourDivergence` and other functions. When set to `True`, all loop integrals that do not contain a `FeynAmpDenominator`, i.e. consist of only scalar products but no denominators, are set to zero.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopIsolate.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopIsolate[SPD[l,q],{q}]


FCLoopIsolate[SPD[l,q],{q},DropScaleless->True]
