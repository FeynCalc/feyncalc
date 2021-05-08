 
(* ::Section:: *)
(* ToHypergeometric *)
(* ::Text:: *)
(*ToHypergeometric[t^b (1 - t)^c (1+tz)^a,t] returns $\text{Null}$. Remember that Re b >0 and Re (c-b) > 0 should hold (need not be set in Mathematica)..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*HypergeometricAC, HypergeometricIR, HypergeometricSE.*)



(* ::Subsection:: *)
(* Examples *)



ToHypergeometric[t^b (1-t)^c (1+t z)^a,t]

ToHypergeometric[w t^(b-1) (1-t)^(c-b-1) (1-t z)^-a,t]

ToHypergeometric[t^b (1-t)^c (u+t z)^a,t]

ToHypergeometric[w t^(b-1) (1-t)^(c-b-1) (u-t z)^-a,t]
