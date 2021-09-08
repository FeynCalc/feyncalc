(* ::Package:: *)

 


(* ::Section:: *)
(*Combine*)


(* ::Text:: *)
(*`Combine[expr]` puts terms in a sum over a common denominator and cancels factors in the result. `Combine` is similar to `Together`, but accepts the option `Expanding` and works usually better than `Together` for polynomials involving rationals with sums in the denominator.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).*)


(* ::Subsection:: *)
(*Examples*)


Combine[((a-b) (c-d))/e+g]


(* ::Text:: *)
(*Here the result from `Together` where the numerator is automatically expanded.*)


Together[((a-b) (c-d))/e+g]


(* ::Text:: *)
(*If the option `Expanding` is set to `True`, the result of `Combine` is the same as `Together`, but uses a slightly different algorithm.*)


Combine[((a-b) (c-d))/e+g,Expanding->True]
