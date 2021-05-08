 
(* ::Section:: *)
(* Tdec *)
(* ::Text:: *)
(*Tdec[{q, mu}, {p}] ; Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] or Tdec[exp, {{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] calculates the tensorial decomposition formulas for Lorentzian integrals. The more common ones are saved in TIDL..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*TID, TIDL, OneLoopSimplify.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*Check that $int d^Df(p,q)q^{mu }= frac{p^{mu }}{p^2}int d^Df(p,q)pcdot q$*)


Tdec[{q,\[Mu]},{p}]

%[[2]]/.%[[1]]


(* ::Text:: *)
(*This calculates integral transformation for any $int d^Dq_1d^Dq_2d^Dq_3$ $fleft(p,q_{1,}q_2,q_3right) q_1^{mu }q_2^{nu }q_3^{rho }$.*)


Tdec[{{Subscript[q, 1],\[Mu]},{Subscript[q, 2],\[Nu]},{Subscript[q, 3],\[Rho]}},{p},List->False]

Contract[% FVD[p,\[Mu]]FVD[p,\[Nu]] FVD[p,\[Rho]]]//Factor
