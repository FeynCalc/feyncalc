(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianPropagatorDenominator*)


(* ::Text:: *)
(*`CartesianPropagatorDenominator[propSq  + ..., propEik + ..., m^2, {n, s}]` encodes a generic Cartesian propagator denominator of the form $\frac{1}{[(q1+...)^2 + q1.p1 + ... + m^2 + s*I \eta]^n}$*)


(* ::Text:: *)
(*`propSq` should be of the form `CartesianMomentum[q1, D - 1]`, while `propEik` should look like `CartesianPair[CartesianMomentum[q1, D - 1], CartesianMomentum[p1, D - 1]`.*)


(* ::Text:: *)
(*`CartesianPropagatorDenominator` is an internal object. To enter such propagators in FeynCalc you should use `CFAD`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CFAD](CFAD.md), [FeynAmpDenominator](FeynAmpDenominator.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Standard $3$-dimensional Cartesian propagator*)


FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p,D-1],0,m^2,{1,-1}]]


(* ::Text:: *)
(*Here we switch the sign of the mass term*)


FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p,D-1],0,-m^2,{1,-1}]]


(* ::Text:: *)
(*And here also the sign of $i \eta$*)


FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p,D-1],0,-m^2,{1,+1}]]


(* ::Text:: *)
(*Eikonal Cartesian propagator with a residual mass term*)


FeynAmpDenominator[CartesianPropagatorDenominator[0,
CartesianPair[CartesianMomentum[p,D-1],CartesianMomentum[q,D-1]],m^2,{1,-1}]]
