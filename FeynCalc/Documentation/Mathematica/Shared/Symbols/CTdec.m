(* ::Package:: *)

 


(* ::Section:: *)
(*CTdec*)


(* ::Text:: *)
(*`CTdec[{{qi, a}, {qj, b}, ...}, {p1, p2, ...}]` or `CTdec[exp, {{qi, a}, {qj, b}, ...}, {p1, p2, ...}]` calculates the tensorial decomposition formulas for Cartesian integrals. The more common ones are saved in TIDL.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Tdec](Tdec.md), [TIDL](TIDL.md), [TID](TID.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Check that $\int d^{D-1} q \, f(p,q) q^i =  \frac{p^i}{p^2} \int d^{D-1} q \, f(p,q) p \cdot q$*)


CTdec[{{q,i}},{p}]


%[[2]]/.%[[1]]


CTdec[{{q,i}},{p},List->False]


(* ::Text:: *)
(*This calculates integral transformation for any $\int d^{D-1} q_1 d^{D-1} q_2 d^{D-1} q_3 f (p, q_1, q_2, q_3) q_1^i q_2^j q_3^k$.*)


CTdec[{{Subscript[q, 1],i},{Subscript[q, 2],j},{Subscript[q, 3],k}},{p},List->False]


Contract[% CVD[p,i]CVD[p,j] CVD[p,k]]//Factor
