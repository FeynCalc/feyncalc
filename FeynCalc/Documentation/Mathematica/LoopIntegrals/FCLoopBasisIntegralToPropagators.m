(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisIntegralToPropagators*)


(* ::Text:: *)
(*`FCLoopBasisIntegralToPropagators[int, {q1, q2, ...}]` is an auxiliary function that converts the loop integral `int` that depends on the loop momenta `q1, q2, ...` to a list of propagators and scalar products. *)


(* ::Text:: *)
(*All propagators and scalar products that do not depend on the loop momenta are discarded, unless the `Rest` option is set to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)


(* ::Subsection:: *)
(*Examples*)


SFAD[p1]

FCLoopBasisIntegralToPropagators[%,{p1}]


SFAD[p1,p2]

FCLoopBasisIntegralToPropagators[%,{p1,p2}]


(* ::Text:: *)
(*If the integral contains propagators raised to integer powers, only one propagator will appear in the output.*)


int=SPD[q,p] SFAD[q,q-p,q-p]


FCLoopBasisIntegralToPropagators[int,{q}]


(* ::Text:: *)
(*However, setting the option `Tally` to `True` will count the powers of the appearing propagators.*)


FCLoopBasisIntegralToPropagators[int,{q},Tally->True]


(* ::Text:: *)
(*Here is a more realistic 3-loop example*)


int=SFAD[{{-k1,0},{mc^2,1},1}] SFAD[{{-k1-k2,0},{mc^2,1},1}]SFAD[{{-k2,0},{0,1},1}] SFAD[{{-k2,0},{0,1},2}] SFAD[{{-k3,0},{mc^2,1},1}] *SFAD[{{k1-k3-p1,0},{0,1},1}] SFAD[{{-k1-k2+k3+p1,0},{0,1},1}] SFAD[{{-k1-k2+k3+p1,0},{0,1},2}]


FCLoopBasisIntegralToPropagators[int,{k1,k2,k3},Tally->True]
