(* ::Package:: *)

 


(* ::Section:: *)
(*FactoringDenominator*)


(* ::Text:: *)
(*`FactoringDenominator` is an option for `Collect2`. It is taken into account only when the option `Numerator` is set to `True`. If `FactoringDenominator` is set to any function `f`, this function will be applied to the denominator of the fraction. The default value is `False`, i.e. the denominator will be left unchanged.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=(x1 a^2 + y 1 a^2 + 2 a b + x2 b^2 + y2 b^2)/(a + b + c^2 +
	2 c d + d^2)


Collect2[ex, a, b]


Collect2[ex, a, b, Numerator->True]


Collect2[ex, a, b, Numerator->True, FactoringDenominator -> Simplify]
