(* ::Package:: *)

 


(* ::Section:: *)
(* ToPaVe *)


(* ::Text:: *)
(*`ToPaVe[exp, q]`  converts all scalar 1-loop integrals in `exp` that depend on the momentum `q` to scalar Passarino Veltman functions `A0`, `B0`, `C0`, `D0` etc.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*PaVeToABCD, ToPaVe2, A0, A00, B0, B1, B00, B11, C0, D0.*)


(* ::Subsection:: *)
(* Examples *)


FAD[{q,m1}]
ToPaVe[%,q]


FAD[{q,m1},{q+p1,m2}]
ToPaVe[%,q]
%//StandardForm


FAD[{q,m1},{q+p1,m2},{q+p2,m3},{q+p3,m4},{q+p4,m5}]
ToPaVe[%,q]


(* ::Text:: *)
(*By default, `ToPaVe` has the option `PaVeToABCD ` set to `True`. This means that some of the `PaVe` functions are automatically converted to direct Passarino-Veltman functions (`A0`,  `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`). This also has consequences for `TID`*)


TID[FVD[q,mu]FAD[{q,m1},{q+p}],q,ToPaVe->True]
%//StandardForm


(* ::Text:: *)
(*If you want to avoid direct functions in the output of `TID` and other functions that employ `ToPaVe`, you need to set the option `PaVeToABCD` to `False` globally.*)


SetOptions[ToPaVe,PaVeToABCD->False];


TID[FVD[q,mu]FAD[{q,m1},{q+p}],q,ToPaVe->True]
%//StandardForm



