(* ::Package:: *)

 


(* ::Section:: *)
(*ToGFAD*)


(* ::Text:: *)
(*`ToGFAD[exp]` converts all occurring propagator types (`FAD`, `SFAD`, `CFAD`) to `GFAD`s. This is mainly useful when doing expansions in kinematic invariants, where e.g. scalar products may not be appear explicitly when using `FAD`- or `SFAD`-notation.*)


(* ::Text:: *)
(*`ToGFAD` is the inverse operation to `FromGFAD`.*)


(* ::Text:: *)
(*Using the option "OnlyMixedQuadraticEikonalPropagators" one can limit the conversion to a particular type of standard and Cartesian propagator denominators that contain both quadratic and eikonal pieces. Those are the ones that usually cause issues when doing topology minimization*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md), [FromGFAD](FromGFAD.md)*)


(* ::Subsection:: *)
(*Examples*)


ToGFAD[FAD[p]]


ToGFAD[FAD[p]]//StandardForm


ToGFAD[SFAD[{p+q,m^2}]]


ToGFAD[SFAD[{p+q,m^2}]]//StandardForm


ToGFAD[SFAD[{p+q,m^2}],FinalSubstitutions->{SPD[q]->0}]


ToGFAD[SFAD[{p+q,m^2}],FinalSubstitutions->{SPD[q]->0}]//StandardForm


(* ::Text:: *)
(*This is not a mixed quadratic-eikonal propagator so it remains unchanged*)


ToGFAD[SFAD[{{k2,0},{0,1},1}],"OnlyMixedQuadraticEikonalPropagators"->True,
FCE->True]//StandardForm


(* ::Text:: *)
(*This is a mixed  propagator that will be converted to a `GFAD`*)


ToGFAD[SFAD[{{k1,2 gkin meta k1 . n-2 gkin meta u0b k1 . n-meta u0b k1 . nb},
{2 gkin meta^2 u0b-2 gkin meta^2 u0b^2,1},1}],
"OnlyMixedQuadraticEikonalPropagators"->True,FCE->True]//StandardForm
