 
(* ::Section:: *)
(*GenPaVe*)
(* ::Text:: *)
(*`GenPaVe[i, j, ..., {{0, m0}, {Momentum[p1], m1}, {Momentum[p2], m2}, ...]` denotes the invariant (and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of the tensor integral decomposition. In contrast to `PaVe` which uses the LoopTools convention,  masses and external momenta in `GenPaVe` are written in the same order as they appear in the original tensor integral, i.e. `FAD[{q,m0},{q-p1,m1},{q-p2,m2},...]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PaVe](PaVe.md).*)


(* ::Subsection:: *)
(*Examples*)


FVD[q,\[Mu]] FVD[q,\[Nu]] FAD[{q,m0},{q+p1,m1},{q+p2,m2}]/(I*Pi^2)
TID[%,q,UsePaVeBasis->True]
TID[%%,q,UsePaVeBasis->True,GenPaVe->True]
