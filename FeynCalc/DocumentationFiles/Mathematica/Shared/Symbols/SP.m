(* ::Package:: *)

 


(* ::Section:: *)
(*SP*)


(* ::Text:: *)
(*`SP[a, b]` denotes a $4$-dimensional scalar product. `SP[a, b]` is transformed into `ScalarProduct[a, b]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*`SP[p]` is the same as `SP[p, p]` $(=p^2)$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [ExpandScalarProduct](ExpandScalarProduct.md), [ScalarProduct](ScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


SP[p,q] + SP[q]


SP[p-q,q+2p]


Calc[ SP[p-q,q+2p] ]


ExpandScalarProduct[SP[p-q]]


SP[a,b]//StandardForm


SP[a,b]//FCI//StandardForm


SP[a,b]//FCI//FCE//StandardForm
