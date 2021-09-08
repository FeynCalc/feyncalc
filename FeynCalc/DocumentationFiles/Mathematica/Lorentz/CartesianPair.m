(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianPair*)


(* ::Text:: *)
(*`CartesianPair[a, b]` is a special pairing used in the internal representation. `a` and `b` may have heads `CartesianIndex` or `CartesianMomentum`. If both `a` and `b` have head `CartesianIndex`, the Kronecker delta is understood. If `a` and `b` have head `CartesianMomentum`, a Cartesian scalar product is meant. If one of `a` and `b` has head `CartesianIndex` and the other `CartesianMomentum`, a Cartesian vector $p^i$ is understood.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [TemporalPair](TemporalPair.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This represents a three-dimensional Kronecker delta*)


CartesianPair[CartesianIndex[i],CartesianIndex[j]]


(* ::Text:: *)
(*This is a $D-1$-dimensional Kronecker delta*)


CartesianPair[CartesianIndex[i,D-1],CartesianIndex[j,D-1]]


(* ::Text:: *)
(*If the Cartesian indices live in different dimensions, this gets resolved according to the t'Hoft-Veltman-Breitenlohner-Maison prescription*)


CartesianPair[CartesianIndex[i,D-1],CartesianIndex[j]]


CartesianPair[CartesianIndex[i,D-1],CartesianIndex[j,D-4]]


CartesianPair[CartesianIndex[i],CartesianIndex[j,D-4]]


(* ::Text:: *)
(*A $3$-dimensional Cartesian vector*)


CartesianPair[CartesianIndex[i],CartesianMomentum[p]]


(* ::Text:: *)
(*A $D-1$-dimensional Cartesian vector*)


CartesianPair[CartesianIndex[i,D-1],CartesianMomentum[p,D-1]]


(* ::Text:: *)
(*$3$-dimensional scalar products of Cartesian vectors*)


CartesianPair[CartesianMomentum[q],CartesianMomentum[p]]


CartesianPair[CartesianMomentum[p],CartesianMomentum[p]]


CartesianPair[CartesianMomentum[p-q],CartesianMomentum[p]]


CartesianPair[CartesianMomentum[p],CartesianMomentum[p]]^2


CartesianPair[CartesianMomentum[p],CartesianMomentum[p]]^3


ExpandScalarProduct[CartesianPair[CartesianMomentum[p-q],CartesianMomentum[p]]]


CartesianPair[CartesianMomentum[-q],CartesianMomentum[p]] + CartesianPair[CartesianMomentum[q],CartesianMomentum[p]]
