(* ::Package:: *)

 


(* ::Section:: *)
(*ToGFAD*)


(* ::Text:: *)
(*`ToGFAD[exp]` converts all occurring propagator types (`FAD`, `SFAD`, `CFAD`) to `GFAD`s. This is mainly useful when doing expansions in kinematic invariants, where e.g. scalar products may not be appear explicitly when using `FAD`- or `SFAD`-notation.*)


(* ::Text:: *)
(*ToGFAD is the inverse operation to FromGFAD.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md), [FromGFAD](FromGFAD.md)*)


(* ::Subsection:: *)
(*Examples*)


ToGFAD[FAD[p]]
%//StandardForm


ToGFAD[SFAD[{p+q,m^2}]]
%//StandardForm


ToGFAD[SFAD[{p+q,m^2}],FinalSubstitutions->{SPD[q]->0}]
%//StandardForm



