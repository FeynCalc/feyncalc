(* ::Package:: *)

 


(* ::Section:: *)
(*ScalarProductCancel*)


(* ::Text:: *)
(*`ScalarProductCancel[exp, q1, q2, ...]` cancels scalar products with propagators.*)


(* ::Text:: *)
(*`ScalarProductCancel[exp]` cancels simple cases.*)


(* ::Text:: *)
(*`ScalarProductCancel` is deprecated, please use the more powerful `ApartFF` instead.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ApartFF](ApartFF.md), [FCClearScalarProducts](FCClearScalarProducts.md), [ExpandScalarProduct](ExpandScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPC](SPC.md), [SPD](SPD.md).*)


(* ::Subsection:: *)
(*Examples*)


SPD[q,p] FAD[{q,m},{q-p,0}]
ScalarProductCancel[%,q]


SPD[q2,p]SPD[q1,p] FAD[{q1,m},{q2,m},q1-p,q2-p,q2-q1]//FCI
SPC[%,q1,q2,FDS->True]
