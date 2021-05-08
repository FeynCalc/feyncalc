 
(* ::Section:: *)
(* ScalarProductCancel *)
(* ::Text:: *)
(*ScalarProductCancel[exp, q1, q2, ...] cancels scalar products with propagators. ScalarProductCancel[exp] cancels simple cases.ScalarProductCancel is deprecated, please use the more powerful ApartFF instead!.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*ApartFF, FCClearScalarProducts, ExpandScalarProduct, Pair, SP, SPC, SPD.*)



(* ::Subsection:: *)
(* Examples *)



SPD[q,p] FAD[{q,m},{q-p,0}]

ScalarProductCancel[%,q]

 SPD[q2,p]SPD[q1,p] FAD[{q1,m},{q2,m},q1-p,q2-p,q2-q1]//FCI

SPC[%,q1,q2,FDS->True]
