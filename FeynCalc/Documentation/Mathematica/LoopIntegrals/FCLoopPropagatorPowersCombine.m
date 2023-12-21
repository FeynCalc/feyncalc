(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopPropagatorPowersCombine*)


(* ::Text:: *)
(*`FCLoopPropagatorPowersCombine[exp]` combines the same propagators in a `FeynAmpDenominator` to one propagator raised to an integer power.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopPropagatorPowersExpand](FCLoopPropagatorPowersExpand.md).*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{{q,0},{m,1},1},{{q,0},{m,1},1}]

ex=FCLoopPropagatorPowersCombine[%]


ex//StandardForm


SFAD[{{q,0},{m,1},-1},{{q,0},{m,1},1}]

ex=FCLoopPropagatorPowersCombine[%]


ex//StandardForm


(* ::Text:: *)
(*The function automatically employs `FeynAmpDenominatorCombine`.*)


int=SFAD[{{-k1,0},{mc^2,1},1}]  SFAD[{{-k1-k2+k3+p1,0},{0,1},1}] SFAD[{{-k1-k2+k3+p1,0},{0,1},2}]


int//FCI//StandardForm


res=FCLoopPropagatorPowersCombine[int]


res//FCI//StandardForm
