(* ::Package:: *)

 


(* ::Section:: *)
(* PaVeToABCD *)


(* ::Text:: *)
(*`PaVeToABCD[expr]` converts suitable PaVe functions to direct Passarino-Veltman functions (`A0`,  `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`). `PaVeToABCD` is nearly the inverse of `ToPaVe2`.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*ToPaVe, ToPaVe2, A0, A00, B0, B1, B00, B11, C0, D0.*)


(* ::Subsection:: *)
(* Examples *)


PaVe[0,{pp},{m1^2,m2^2}]
PaVeToABCD[%]
%//FCI//StandardForm


PaVe[0,{SPD[p1],0,SPD[p2]},{m1^2,m2^2,m3^2}]
PaVeToABCD[%]
%//FCI//StandardForm



PaVe[0,0,{SPD[p1],0,SPD[p2]},{m1^2,m2^2,m3^2}]
PaVeToABCD[%]
%//FCI//StandardForm



