(* ::Package:: *)

 


(* ::Section:: *)
(*StandardPropagatorDenominator*)


(* ::Text:: *)
(*`StandardPropagatorDenominator[Momentum[q1, D] +..., Pair[Momentum[q1, D], Momentum[p1, D] +..., m^2, {n, s}]` encodes a generic Lorentzian propagator denominator $\frac{1}{[(q_1+ \ldots)^2 + q_1 \cdot p_1 + \ldots + m^2 + s i \eta]^n}$.*)


(* ::Text:: *)
(*This allows to accommodate for standard propagators of the type $1/(p^2-m^2)$ but also for propagators encountered in manifestly Lorentz covariant effective field theories such as HQET or SCET.*)


(* ::Text:: *)
(*`StandardPropagatorDenominator` is an internal object. To enter such propagators in FeynCalc you should use `SFAD`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PropagatorDenominator](PropagatorDenominator.md), [CartesianPropagatorDenominator](CartesianPropagatorDenominator.md), [GenericPropagatorDenominator](GenericPropagatorDenominator.md), [FeynAmpDenominator](FeynAmpDenominator.md).*)


(* ::Subsection:: *)
(*Examples*)


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,-m^2,{1,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[0,Pair[Momentum[p,D],Momentum[q,D]],-m^2,{1,1}]]
