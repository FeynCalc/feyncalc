 
(* ::Section:: *)
(*C0*)
(* ::Text:: *)
(*`C0[p10, p12, p20, m1^2, m2^2, m3^2]` is the scalar Passarino-Veltman $C_0$ function. The convention for the arguments is that if the denominator of the integrand has the form $([q^2-m1^2] [(q+p1)^2-m2^2] [(q+p2)^2-m3^2])$, the first three arguments of C0 are the scalar products $p10 = p1^2$, $p12 = (p1-p2).(p1-p2)$, $p20 = p2^2$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B0](B0.md), [D0](D0.md), [PaVe](PaVe.md), [PaVeOrder](PaVeOrder.md).*)



(* ::Subsection:: *)
(*Examples*)


C0[a,b,c, m12,m22,m32]


C0[b,a,c,m32,m22,m12]//PaVeOrder


PaVeOrder[C0[b,a,c,m32,m22,m12],PaVeOrderList->{c,a}]
