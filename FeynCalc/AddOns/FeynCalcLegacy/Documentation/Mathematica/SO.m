(* ::Package:: *)

 


(* ::Section:: *)
(*SO*)


(* ::Text:: *)
(*`SO[q]` is a four-dimensional scalar product of `OPEDelta` with `q`. It is transformed into `Pair[Momentum[q], Momentum[OPEDelta]` by `FCI`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCI](FCI.md), [OPEDelta](OPEDelta.md), [Pair](Pair.md), [ScalarProduct](ScalarProduct.md), [SOD](SOD.md).*)


(* ::Subsection:: *)
(*Examples*)


SO[p]


SO[p-q]


SO[p]//FCI//StandardForm
