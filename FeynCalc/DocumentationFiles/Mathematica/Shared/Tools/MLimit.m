 
(* ::Section:: *)
(* MLimit *)
(* ::Text:: *)
(*MLimit[expr, lims] takes multiple limits of expr using the limits lims..*)


(* ::Subsection:: *)
(* Examples *)
MLimit[y Log[y]+Sin[x-1]/(x-1),{x->1,y->0}]
