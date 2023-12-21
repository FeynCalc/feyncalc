(* ::Package:: *)

 


(* ::Section:: *)
(*ToPaVe2*)


(* ::Text:: *)
(*`ToPaVe2[expr]` converts all the direct Passarino-Veltman functions (`A0`, `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0`) to `PaVe`-functions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md).*)


(* ::Subsection:: *)
(*Examples*)


ToPaVe2[A0[m^2]]


ToPaVe2[A0[m^2]]//FCI//StandardForm


ToPaVe2[B11[pp,m^2,M^2,BReduce->False]]


ToPaVe2[B11[pp,m^2,M^2,BReduce->False]]//FCI//StandardForm
