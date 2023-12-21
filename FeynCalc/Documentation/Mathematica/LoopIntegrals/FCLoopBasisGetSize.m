(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisGetSize*)


(* ::Text:: *)
(*`FCLoopBasisGetSize[n1, n2]` returns the number of linearly independent propagators for a topology that contains `n1` loop momenta and `n2` external momenta.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FCLoopBasisGetSize[1,0]


FCLoopBasisGetSize[2,1]


FCLoopBasisGetSize[3,2]


FCLoopBasisGetSize[4,1]


(* ::Text:: *)
(*The third argument (if given) is simply added to the final result.*)


FCLoopBasisGetSize[4,1,1]
