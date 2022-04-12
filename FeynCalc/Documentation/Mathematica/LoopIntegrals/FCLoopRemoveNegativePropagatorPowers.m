(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopRemoveNegativePropagatorPowers*)


(* ::Text:: *)
(*`FCLoopRemoveNegativePropagatorPowers[exp]` rewrites propagators raised to integer powers as products.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{q,m,-1}]

ex=FCLoopRemoveNegativePropagatorPowers[%]


ex//StandardForm


SFAD[{q,m},q+p,{q,m,-2}]

ex=FCLoopRemoveNegativePropagatorPowers[%]


ex//StandardForm
