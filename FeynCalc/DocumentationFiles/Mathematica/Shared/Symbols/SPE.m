(* ::Package:: *)

 


(* ::Section:: *)
(*SPE*)


(* ::Text:: *)
(*`SPE[a, b]` denotes a $D-4$-dimensional scalar product. `SPE[a, b]` is transformed into `Pair[Momentum[a, -4 + D], Momentum[b, -4 + D]]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*`SPE[p]` is the same as `SPE[p,p]`  $(=p^2)$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PD](PD.md), [Calc](Calc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProduct](ScalarProduct.md), [SPD](SPD.md).*)


(* ::Subsection:: *)
(*Examples*)


SPE[p,q] + SPE[q]


SPE[p-q,q+2p]


Calc[ SPE[p-q,q+2p] ]


ExpandScalarProduct[SPE[p-q]]


SPE[a,b]//StandardForm


SPE[a,b]//FCI//StandardForm


SPE[a,b]//FCI//FCE//StandardForm


FCE[ChangeDimension[SP[p,q], D-4]]//StandardForm
