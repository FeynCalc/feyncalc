 
(* ::Section:: *)
(* ThreeDivergence *)
(* ::Text:: *)
(*ThreeDivergence[exp, CV[p, i]]  calculates the partial derivative of exp w.r.t. $p^i$. ThreeDivergence[exp, CV[p, i], CV[p,i], ...] gives the multiple derivative..*)


(* ::Subsection:: *)
(* Examples *)
CSP[p,q]

ThreeDivergence[%,CV[q,i]]

CSP[p-k,q]

ThreeDivergence[%,CV[k,i]]

CFAD[{p,m^2}]

ThreeDivergence[%,CVD[p,i]]
