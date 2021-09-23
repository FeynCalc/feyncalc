(* ::Package:: *)

 


(* ::Section:: *)
(*FCDuplicateFreeQ*)


(* ::Text:: *)
(*`FCDuplicateFreeQ[list]` yields `True` if list contains no duplicates and `False` otherwise.*)


(* ::Text:: *)
(*`FCDuplicateFreeQ[list,test]` uses test to determine whether two objects should be considered duplicates.*)


(* ::Text:: *)
(*`FCDuplicateFreeQ` returns the same results as the standard `DuplicateFreeQ`. The only reason for introducing `FCDuplicateFreeQ` is that `DuplicateFreeQ` is not available in Mathematica 8 and 9, which are still supported by FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCSubsetQ](FCSubsetQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FCDuplicateFreeQ[{a,b,c}]


FCDuplicateFreeQ[{a,b,c,a}]


FCDuplicateFreeQ[{{a,b},{a,c}}]


FCDuplicateFreeQ[{{a,b},{a,c}},Function[{x,y},First[x]===First[y]]]
