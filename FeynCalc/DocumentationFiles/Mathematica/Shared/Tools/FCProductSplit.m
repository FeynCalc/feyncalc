(* ::Package:: *)

 


(* ::Section:: *)
(*FCProductSplit*)


(* ::Text:: *)
(*`FCProductSplit[exp, {v1, v2, ...}]` splits expr into pieces that are free of any occurrence of `v1, v2, ...` and pieces that contain those variables. This works both on sums and products. The output is provided in the form of a two element list. One can recover the original expression by applying `Total` to that list.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCSplit](FCSplit.md).*)


(* ::Subsection:: *)
(*Examples*)


FCProductSplit[c^2,{a}]


FCProductSplit[a^2*b,{a}]


FCProductSplit[(a^2+b)*b*(c+d),{a,c}]
