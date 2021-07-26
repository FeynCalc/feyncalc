(* ::Package:: *)

(* ::Section:: *)
(*FactorFull*)


(* ::Text:: *)
(*`FactorFull` is an option of `Factor2` (default `False`). If set to `False`, products like `(a-b) (a+b)` will be replaced by `a^2-b^2`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Factor2](Factor2).*)


(* ::Subsection:: *)
(*Examples*)


Factor2[(a-b)(a+b)(c+d)(c-d)]


Factor2[(a-b)(a+b)(c+d)(c-d),FactorFull->True]
