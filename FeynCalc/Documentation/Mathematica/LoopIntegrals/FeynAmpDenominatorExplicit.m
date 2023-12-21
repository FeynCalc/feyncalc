(* ::Package:: *)

 


(* ::Section:: *)
(*FeynAmpDenominatorExplicit*)


(* ::Text:: *)
(*`FeynAmpDenominatorExplicit[exp]` changes each occurrence of `PropagatorDenominator[a,b]` in exp into `1/(SPD[a,a]-b^2)` and replaces `FeynAmpDenominator` by `Identity`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominator](FeynAmpDenominator.md), [PropagatorDenominator](PropagatorDenominator.md).*)


(* ::Subsection:: *)
(*Examples*)


FAD[{q,m},{q-p,0}]

FeynAmpDenominatorExplicit[%]

%//FCE//StandardForm


(* ::Text:: *)
(*Notice that you should never apply `FeynAmpDenominatorExplicit` to loop integrals. Denominators in a proper loop integral should be written as `FeynAmpDenominator`s. Otherwise, the given integral is assumed to have no denominators and consequently set to zero as being scaleless.*)


TID[FVD[q,mu]FAD[{q,m},{q-p,0}],q]


TID[FeynAmpDenominatorExplicit[FVD[q,mu]FAD[{q,m},{q-p,0}]],q]
