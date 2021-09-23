(* ::Package:: *)

 


(* ::Section:: *)
(*GenericPropagatorDenominator*)


(* ::Text:: *)
(*`GenericPropagatorDenominator[expr, {n, s}]`  is a generic factor of the denominator of a propagator. Unlike `PropagatorDenominator` that is supposed to mean $1/(q^2-m^2)$, `expr` in `GenericPropagatorDenominator` can be an arbitrary combination of `Pair`, `CartesianPair` and `TemporalPair` objects.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PropagatorDenominator](PropagatorDenominator.md), [StandardPropagatorDenominator](StandardPropagatorDenominator.md), [CartesianPropagatorDenominator](CartesianPropagatorDenominator.md).*)


(* ::Text:: *)
(*Using `n` one can specify the power of the propagator, while `s` (`+1` or `-1`) fixes the sign of `I*eta`. `GenericPropagatorDenominator` is an internal object. To enter such propagators in FeynCalc you should use `GFAD`.*)


(* ::Subsection:: *)
(*Examples*)


FeynAmpDenominator[GenericPropagatorDenominator[x,{1,1}]]


FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1,D],
Momentum[Q,D]]Pair[Momentum[p2,D],Momentum[Q,D]]-Pair[Momentum[p1,D],
Momentum[p2,D]],{1,1}]]
