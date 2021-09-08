(* ::Package:: *)

 


(* ::Section:: *)
(*FCProgressBar*)


(* ::Text:: *)
(*`FCProgressBar[text, i, total]`  is a simple auxiliary function that can be used to display the progress of a certain evaluation, e.g. mapping a list of integrals to some function. Here i is the number of the current step while total denotes the overall number of steps.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A simple usage example*)


Table[FCProgressBar["Calculating integral ", i, 10], {i, 1, 10}];
