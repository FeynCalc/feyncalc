(* ::Package:: *)

 


(* ::Section:: *)
(*FCReorderList*)


(* ::Text:: *)
(*`FCReorderList[li, ord]` reorders the list `li` according to the given ordering `ord`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


myList=Table[mat[i], {i, 1, 23}]


FCReorderList[myList, {{1, 10}, 23, {11, 20}, 22, 21}]
