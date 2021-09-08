(* ::Package:: *)

 


(* ::Section:: *)
(*$AL*)


(* ::Text:: *)
(*`$AL` is the head for dummy indices which may be introduced by `Amputate` and `Uncontract`. By default it is unset, but may be set to anything.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Amputate](Amputate.md), [Uncontract](Uncontract.md).*)


(* ::Subsection:: *)
(*Examples*)


Uncontract[ScalarProduct[p,q],q,Pair->All]


$AL=\[Mu];
Uncontract[ScalarProduct[p,q],q,Pair->All]


$AL=.;
