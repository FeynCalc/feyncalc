(* ::Package:: *)

 


(* ::Section:: *)
(*FeynAmpDenominatorSplit*)


(* ::Text:: *)
(*`FeynAmpDenominatorSplit[expr]` splits all `FeynAmpDenominator[a,b, ...]` in `expr` into `FeynAmpDenominator[a]*FeynAmpDenominator[b]*...` . `FeynAmpDenominatorSplit[expr,  Momentum ->q1]` splits all `FeynAmpDenominator` in expr into two products, one containing `q1` and other momenta, the second being free of `q1`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine.md).*)


(* ::Subsection:: *)
(*Examples*)


FAD[q1,q1-p,q1-q2,q2,q2-p]

ex=FeynAmpDenominatorSplit[%]


ex//FCE//StandardForm


ex=FeynAmpDenominatorSplit[FAD[q1,q1-p,q1-q2,q2,q2-p],Momentum->{q1}]


ex//FCE//StandardForm


FeynAmpDenominatorCombine[ex]//FCE//StandardForm
