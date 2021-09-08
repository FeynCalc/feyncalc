(* ::Package:: *)

 


(* ::Section:: *)
(*PaVeToABCD*)


(* ::Text:: *)
(*`PaVeToABCD[expr]` converts suitable PaVe functions to direct Passarino-Veltman functions (`A0`,  `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`). `PaVeToABCD` is nearly the inverse of `ToPaVe2`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md), [ToPaVe2](ToPaVe2.md), [A0](A0.md), [A00](A00.md), [B0](B0.md), [B1](B1.md), [B00](B00.md), [B11](B11.md), [C0](C0.md), [D0](D0.md).*)


(* ::Subsection:: *)
(*Examples*)


PaVe[0,{pp},{m1^2,m2^2}]
PaVeToABCD[%]
%//FCI//StandardForm


PaVe[0,{SPD[p1],0,SPD[p2]},{m1^2,m2^2,m3^2}]
PaVeToABCD[%]
%//FCI//StandardForm



PaVe[0,0,{SPD[p1],0,SPD[p2]},{m1^2,m2^2,m3^2}]
PaVeToABCD[%]
%//FCI//StandardForm



