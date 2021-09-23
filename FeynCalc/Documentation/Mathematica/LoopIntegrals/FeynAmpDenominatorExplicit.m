 
(* ::Section:: *)
(*FeynAmpDenominatorExplicit*)
(* ::Text:: *)
(*`FeynAmpDenominatorExplicit[exp]` changes each occurence of `PropagatorDenominator[a,b]` in exp into `1/(SPD[a,a]-b^2)` and replaces `FeynAmpDenominator` by `Identity`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominator](FeynAmpDenominator.md), [PropagatorDenominator](PropagatorDenominator.md).*)



(* ::Subsection:: *)
(*Examples*)


FAD[{q,m},{q-p,0}]
FeynAmpDenominatorExplicit[%]
%//FCE//StandardForm
