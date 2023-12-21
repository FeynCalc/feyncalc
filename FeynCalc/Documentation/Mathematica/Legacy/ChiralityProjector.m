(* ::Package:: *)

 


(* ::Section:: *)
(*ChiralityProjector*)


(* ::Text:: *)
(*`ChiralityProjector[+1]` denotes $1/2\left(1+\gamma^5\right)$.*)


(* ::Text:: *)
(*ChiralityProjector[-1] denotes $1/2\left(1+\gamma ^5\right)$.*)


(* ::Text:: *)
(*The shortcut `ChiralityProjector` is deprecated, please use `GA[6]` and `GA[7]` instead!*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GA](GA.md), [FCI](FCI.md).*)


(* ::Subsection:: *)
(*Examples*)


{ChiralityProjector[+1],ChiralityProjector[-1]}
DiracSimplify[#,DiracSubstitute67->True]&/@%


(* ::Text:: *)
(*`ChiralityProjector` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use GA[6] and GA[7].*)


{GA[6],GA[7]}


FCI[GA[6]]===ChiralityProjector[+1]


FCI[GA[7]]===ChiralityProjector[-1]
