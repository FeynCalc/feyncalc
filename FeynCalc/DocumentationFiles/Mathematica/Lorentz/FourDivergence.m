 
(* ::Section:: *)
(* FourDivergence *)
(* ::Text:: *)
(*FourDivergence[exp, FV[p, mu]] calculates the partial derivative of exp w.r.t $p^{\mu }$. FourDivergence[exp, FV[p, mu], FV[p,nu], ...] gives the multiple derivative..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*RussianTrick.*)



(* ::Subsection:: *)
(* Examples *)



SP[p,q]

FourDivergence[%,FV[q,\[Mu]]]

SP[p-k,q]

FourDivergence[%,FV[k,\[Mu]]]

SFAD[{p,m^2}]

FourDivergence[%,FVD[p,\[Nu]]]
