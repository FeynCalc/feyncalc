(* ::Package:: *)

(* ::Section:: *)
(*TemporalPair*)


(* ::Text:: *)
(*`TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]]` is a special pairing used in the internal representation to denote $p^0$, the temporal component of a 4-momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[TemporalPair](TemporalPair), [TC](TC), [ExplicitLorentzIndex](ExplicitLorentzIndex).*)


(* ::Subsection:: *)
(*Examples*)


TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]]


TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p+q]]
%//ExpandScalarProduct
