 
(* ::Section:: *)
(* PaVe *)
(* ::Text:: *)
(*PaVe[i, j, ..., {p10, p12, ...}, {m1^2, mw^2, ...}] denotes the invariant (and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of the tensor integral decomposition. Joining plist and mlist gives the same conventions as for A0, B0, C0, D0. Automatic simlifications are performed for the coefficient functions of two-point integrals and for the scalar integrals..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*PaVeReduce.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*Some of the PaVe's reduce to special cases with $text{PaVeAutoReduce}to text{True}$*)


PaVe[0,0,{pp},{m^2,M^2},PaVeAutoReduce->True]
