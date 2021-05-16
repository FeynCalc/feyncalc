 
(* ::Section:: *)
(* ToPaVe *)
(* ::Text:: *)
(*ToPaVe[exp, q]  converts all scalar 1-loop integrals in exp that depend on the momentum q to scalar Passarino Veltman functions A0, B0, C0, D0 etc..*)


(* ::Subsection:: *)
(* Examples *)


FAD[{q,m1}]
ToPaVe[%,q]


FAD[{q,m1},{q+p1,m2}]
ToPaVe[%,q]


FAD[{q,m1},{q+p1,m2},{q+p2,m3},{q+p3,m4},{q+p4,m5}]
ToPaVe[%,q]
