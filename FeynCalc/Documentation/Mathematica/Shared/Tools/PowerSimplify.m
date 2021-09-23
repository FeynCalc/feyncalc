(* ::Package:: *)

 


(* ::Section:: *)
(*PowerSimplify*)


(* ::Text:: *)
(*`PowerSimplify[exp]` simplifies `(-x)^a` to `(-1)^a x^a` and `(y-x)^n` to `(-1)^n (x-y)^n` thus assuming that the exponent is an integer (even if it is symbolic).*)


(* ::Text:: *)
(*Furthermore, `(-1)^(a+n) ` and `I^(a+n)` are expanded and `(I)^(2 m) -> (-1)^m and (-1)^(n_Integer?EvenQ m) -> 1` and `(-1)^(n_Integer?OddQ m) -> (-1)^m` for `n` even and odd respectively and (-1)^(-n) -> (-1)^n and Exp[I m Pi] -> (-1)^m.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [OPEm](OPEm.md).*)


(* ::Subsection:: *)
(*Examples*)


PowerSimplify[(-1)^(2OPEm)]


PowerSimplify[(-1)^(OPEm+2)]


PowerSimplify[(-1)^(OPEm-2)]


PowerSimplify[I^(2OPEm)]
