(* ::Package:: *)

 


(* ::Section:: *)
(*FCPartialFractionForm*)


(* ::Text:: *)
(*`FCPartialFractionForm[n, {{f1,x-r1,p1},{f2,x-r2,p2}, ...}, x]` is a special way of representing sums of rational functions of `x` given by $n + \frac{f_1}{[x-r_1]^p_1} + \frac{f_2}{[x-r_2]^p_2} + \ldots$*)


(* ::Text:: *)
(*It is inspired by the `parfrac`form from Maple and its usage in E. Panzer's HyperInt for the integration of multiple polylogarithms.*)


(* ::Text:: *)
(*Use `ToFCPartialFractionForm` to convert the given expression to this notation and `FromFCPartialFractionForm` to return back to the usual representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ToFCPartialFractionForm](ToFCPartialFractionForm.md), [FromFCPartialFractionForm](FromFCPartialFractionForm.md).*)


(* ::Subsection:: *)
(*Examples*)


Apart[c+x^2/(x-1),x]


ex1=ToFCPartialFractionForm[c+x^2/(x-1),x]


FromFCPartialFractionForm[ex1]


ex2=ToFCPartialFractionForm[(-64*(-1 + z^2))/(15*(1 + z^2 + z^4)),z]


FromFCPartialFractionForm[ex2]


FromFCPartialFractionForm[ex2,Factoring->Simplify]
