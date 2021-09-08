(* ::Package:: *)

 


(* ::Section:: *)
(*SOD*)


(* ::Text:: *)
(*`SOD[q]` is a $D$-dimensional scalar product of `OPEDelta` with `q`. It is transformed into `Pair[Momentum[q,D], Momentum[OPEDelta,D]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [OPEDelta](OPEDelta.md), [Pair](Pair.md), [ScalarProduct](ScalarProduct.md), [SOD](SOD.md).*)


(* ::Subsection:: *)
(*Examples*)


SOD[p]


SOD[p-q]


SOD[p]//FCI//StandardForm
