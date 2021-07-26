(* ::Package:: *)

(* ::Section:: *)
(*FCLoopPropagatorPowersCombine*)


(* ::Text:: *)
(*`FCLoopPropagatorPowersCombine[exp]` combines the same propagators in a `FeynAmpDenominator` to one propagator raised to an integer power.*)


(* ::Subsection:: *)
(*See also*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{{q,0},{m,1},1},{{q,0},{m,1},1}]
FCLoopPropagatorPowersCombine[%]
%//StandardForm


SFAD[{{q,0},{m,1},-1},{{q,0},{m,1},1}]
FCLoopPropagatorPowersCombine[%]
