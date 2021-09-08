(* ::Package:: *)

 


(* ::Section:: *)
(*FCSubsetQ*)


(* ::Text:: *)
(*`FCSubsetQ[list1, list2]`  yields `True` if `list2` is a subset of `list1` and `False` otherwise. It returns the same results as the standard `SubsetQ`. The only reason for introducing `FCSubsetQ` is that `SubsetQ` is not available in Mathematica 8 and 9, which are still supported by FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCDuplicateFreeQ](FCDuplicateFreeQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FCSubsetQ[{a,b,c,d},{a,d,e}]


FCSubsetQ[{a,b,c,d},{a,d}]
