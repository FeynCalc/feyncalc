(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopPropagatorPowersExpand*)


(* ::Text:: *)
(*`FCLoopPropagatorPowersExpand[exp]` rewrites propagators raised to integer powers as products.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopPropagatorPowersCombine](FCLoopPropagatorPowersCombine.md).*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{q,m,2}]

ex=FCLoopPropagatorPowersExpand[%]


ex//StandardForm


SFAD[{q,m,2},q+p]

ex=FCLoopPropagatorPowersExpand[%]


ex//StandardForm
