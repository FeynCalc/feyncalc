(* ::Package:: *)

(* ::Section:: *)
(*FAD*)


(* ::Text:: *)
(*`FAD` is the FeynCalc external form of `FeynAmpDenominator` and denotes an inverse propagator.*)


(* ::Text:: *)
(*`FAD[q, q-p, ...]` is $\frac{1}{q^2 (q-p)^2 \ldots}$.*)


(* ::Text:: *)
(*`FAD[{q1,m}, {q1-p,m}, q2, ...]` is \frac{1}{[q1^2 - m^2][(q1-p)^2 - m^2] q2^2 }. Translation into FeynCalc internal form is performed by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FAD](FAD), [FCE](FCE), [FCI](FCI), [FeynAmpDenominator](FeynAmpDenominator), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify), [PropagatorDenominator](PropagatorDenominator).*)


(* ::Subsection:: *)
(*Examples*)


FAD[q,p-q]


FAD[p,{p-q,m}]


FAD[{p,0,2},{p-q,m,3}]


FAD[q,p-q]//FCI//StandardForm


FAD[p] FAD[p-q] // FeynAmpDenominatorCombine[#,FCE->True]&//StandardForm
