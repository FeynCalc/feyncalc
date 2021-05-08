 
(* ::Section:: *)
(* TC *)
(* ::Text:: *)
(*TC[p] is the temporal component of a 4-vector and is transformed into TemporalPair[TemporalMomentum[p], ExplicitLorentzIndex[0]] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* Examples *)
TC[p]

TC[p-q]

FCI[TC[p]]//StandardForm


(* ::Text:: *)
(*ExpandScalarProduct is used to expand momenta in TC*)


ExpandScalarProduct[TC[p-q]]
