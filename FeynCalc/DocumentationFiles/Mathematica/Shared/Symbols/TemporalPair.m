 
(* ::Section:: *)
(* TemporalPair *)
(* ::Text:: *)
(*TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]] is a special pairing used in the internal representation to denote p^0, the temporal components of a four momentum p..*)


(* ::Subsection:: *)
(* Examples *)
TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]]

TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p+q]]

%//ExpandScalarProduct
