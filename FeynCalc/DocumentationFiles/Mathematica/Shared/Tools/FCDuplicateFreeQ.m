 
(* ::Section:: *)
(* FCDuplicateFreeQ *)
(* ::Text:: *)
(*FCDuplicateFreeQ[list] yields True if list contains no duplicates and False otherwise. FCDuplicateFreeQ[list,test] uses test to determine whether two objects should be considered duplicates.FCDuplicateFreeQ returns the same results as the standard DuplicateFreeQ. The only reason for introducing FCDuplicateFreeQ is that DuplicateFreeQ is not available in Mathematica 8 and 9, which are still supported by FeynCalc..*)


(* ::Subsection:: *)
(* Examples *)
FCDuplicateFreeQ[{a,b,c}]

FCDuplicateFreeQ[{a,b,c,a}]

FCDuplicateFreeQ[{{a,b},{a,c}}]

FCDuplicateFreeQ[{{a,b},{a,c}},Function[{x,y},First[x]===First[y]]]
