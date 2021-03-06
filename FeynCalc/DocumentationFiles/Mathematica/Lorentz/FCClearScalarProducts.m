 
(* ::Section:: *)
(* FCClearScalarProducts *)
(* ::Text:: *)
(*[]   FCClearScalarProducts[] removes all user-performed specific settings for ScalarProduct's..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*ScalarProduct, Pair, SP, SPD.*)



(* ::Subsection:: *)
(* Examples *)



ScalarProduct[p,p]=m^2

Pair[Momentum[p],Momentum[p]]

FCClearScalarProducts[]
Pair[Momentum[p],Momentum[p]]

SP[p,p]
