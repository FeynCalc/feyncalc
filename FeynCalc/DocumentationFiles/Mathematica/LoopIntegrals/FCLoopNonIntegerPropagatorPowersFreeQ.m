 
(* ::Section:: *)
(*FCLoopNonIntegerPropagatorPowersFreeQ*)
(* ::Text:: *)
(*`FCLoopNonIntegerPropagatorPowersFreeQ[int]` checks if the integral contains propagators raised to noninteger (i.e. fractional or symbolic) powers.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopRemoveNegativePropagatorPowers](FCLoopRemoveNegativePropagatorPowers.md).*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{q+p,m^2,2}]
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]


SFAD[{q+p,m^2,n}]
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]


CFAD[{l,m^2,1/2}]
FCLoopNonIntegerPropagatorPowersFreeQ[FCI[%]]
