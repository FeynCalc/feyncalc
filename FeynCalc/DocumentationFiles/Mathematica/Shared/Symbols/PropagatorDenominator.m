(* ::Package:: *)

 


(* ::Section:: *)
(*PropagatorDenominator*)


(* ::Text:: *)
(*`PropagatorDenominator[Momentum[q], m]`  is a factor of the denominator of a propagator. If `q` is supposed to be $D$-dimensional, use `PropagatorDenominator[Momentum[q, D], m]`. What is meant is $1/(q^2-m^2)$.*)


(* ::Text:: *)
(*` PropagatorDenominator` must appear inside `FeynAmpDenominator`, it is not a standalone object.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominator](FeynAmpDenominator.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md).*)


(* ::Subsection:: *)
(*Examples*)


FeynAmpDenominator[PropagatorDenominator[Momentum[p],m]]


 FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],m]]
