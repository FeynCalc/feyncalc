(* ::Package:: *)

 


(* ::Section:: *)
(*SPD*)


(* ::Text:: *)
(*`SPD[a, b]` denotes a $D$-dimensional scalar product.*)


(* ::Text:: *)
(* `SPD[a, b]` is transformed into `ScalarProduct[a, b,Dimension->D]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*`SPD[p]` is the same as `SPD[p,p]` $(=p^2)$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PD](PD.md), [Calc](Calc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProduct](ScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


SPD[p,q] + SPD[q]


SPD[p-q,q+2p]


Calc[ SPD[p-q,q+2p] ]


ExpandScalarProduct[SPD[p-q]]


SPD[a,b]//StandardForm


SPD[a,b]//FCI//StandardForm


SPD[a,b]//FCI//FCE//StandardForm


FCE[ChangeDimension[SP[p,q], D]]//StandardForm
