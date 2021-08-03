(* ::Package:: *)

 


(* ::Section:: *)
(*SOD*)


(* ::Text:: *)
(*`SOD[q]` is a $D$-dimensional scalar product of `OPEDelta` with `q`. It is transformed into `Pair[Momentum[q,D], Momentum[OPEDelta,D]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[OPEDelta](OPEDelta), [Pair](Pair), [ScalarProduct](ScalarProduct), [SOD](SOD).*)


(* ::Subsection:: *)
(*Examples*)


SOD[p]


SOD[p-q]


SOD[p]//FCI//StandardForm
