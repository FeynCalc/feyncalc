(* ::Package:: *)

 


(* ::Section:: *)
(*TemporalPair*)


(* ::Text:: *)
(*`TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]]` is a special pairing used in the internal representation to denote $p^0$, the temporal component of a 4-momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TemporalPair](TemporalPair.md), [TC](TC.md), [ExplicitLorentzIndex](ExplicitLorentzIndex.md).*)


(* ::Subsection:: *)
(*Examples*)


TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]]


TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p+q]]
%//ExpandScalarProduct
