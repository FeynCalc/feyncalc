 
(* ::Section:: *)
(*FeynAmpDenominatorCombine*)
(* ::Text:: *)
(*`FeynAmpDenominatorCombine[expr]` expands expr with respect to `FeynAmpDenominator` and combines products of `FeynAmpDenominator` in expr into one `FeynAmpDenominator`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorSplit](FeynAmpDenominatorSplit.md).*)



(* ::Subsection:: *)
(*Examples*)


FAD[q] FAD[q-p]
FeynAmpDenominatorCombine[%]
%//FCE//StandardForm


FeynAmpDenominatorSplit[%]
%//FCE//StandardForm
