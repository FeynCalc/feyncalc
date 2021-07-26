(* ::Package:: *)

(* ::Section:: *)
(*GammaEpsilon*)


(* ::Text:: *)
(*`GammaEpsilon[exp]` gives a series expansion of `Gamma[exp]` in `Epsilon` up to order `6` (where `EulerGamma` is neglected).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[GammaExpand](GammaExpand), [Series2](Series2).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*If the argument is of the form `(1+a Epsilon)` the result is not calculated but tabulated.*)


GammaEpsilon[1+a Epsilon]


GammaEpsilon[1-Epsilon/2]


(* ::Text:: *)
(*For other arguments the expansion is calculated.*)


GammaEpsilon[Epsilon]


GammaEpsilon[x]
