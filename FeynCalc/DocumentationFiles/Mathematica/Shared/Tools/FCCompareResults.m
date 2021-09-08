(* ::Package:: *)

 


(* ::Section:: *)
(*FCCompareResults*)


(* ::Text:: *)
(*`FCCompareResults[{res1, res2, ...}, {res1Known, res2Known, ...}]` compares the given list of expression `{res1,res2,...}` to the list of expressions `{res1Known,res2Known,...}` that represents the correct results. This is handy for checking both intermediate and final results of calculations, where you know what should come out at the end.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCMathcSolve](FCMathcSolve.md).*)


(* ::Subsection:: *)
(*Examples*)


FCCompareResults[{4,4},{2^2,8/2}]


FCCompareResults[{3,5},{2^2,8/2}]



