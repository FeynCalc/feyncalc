(* ::Package:: *)

 


(* ::Section:: *)
(*TC*)


(* ::Text:: *)
(*`TC[p]` is the temporal component of a $4$-vector and is transformed into `TemporalPair[TemporalMomentum[p], ExplicitLorentzIndex[0]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TemporalPair](TemporalPair.md), [TemporalMomentum](TemporalMomentum.md), [FCClearScalarProducts](FCClearScalarProducts.md).*)


(* ::Subsection:: *)
(*Examples*)


TC[p]


TC[p-q]


FCI[TC[p]]//StandardForm


(* ::Text:: *)
(*`ExpandScalarProduct` is used to expand momenta in `TC`*)


ExpandScalarProduct[TC[p-q]]
%//StandardForm
