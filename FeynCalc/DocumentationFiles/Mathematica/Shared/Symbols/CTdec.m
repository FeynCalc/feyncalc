 
(* ::Section:: *)
(* CTdec *)
(* ::Text:: *)
(*CTdec[{{qi, a}, {qj, b}, ...}, {p1, p2, ...}] or CTdec[exp, {{qi, a}, {qj, b}, ...}, {p1, p2, ...}] calculates the tensorial decomposition formulas for Cartesian integrals. The more common ones are saved in TIDL..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Tdec.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*Check that $int d^{D-1}f(p,q)q^i= frac{p^i}{p^2}int d^{D-1}f(p,q)pcdot q$*)


CTdec[{{q,i}},{p}]

%[[2]]/.%[[1]]


(* ::Text:: *)
(*This calculates integral transformation for any $int d^{D-1}q_1d^{D-1}q_2d^{D-1}q_3$ $fleft(p,q_{1,}q_2,q_3right) q_1^iq_2^jq_3^k$.*)


CTdec[{{Subscript[q, 1],i},{Subscript[q, 2],j},{Subscript[q, 3],k}},{p},List->False]

Contract[% CVD[p,i]CVD[p,j] CVD[p,k]]//Factor
