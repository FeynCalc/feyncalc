(* ::Package:: *)

 


(* ::Section:: *)
(*ToFCPartialFractionForm*)


(* ::Text:: *)
(*`ToFCPartialFractionForm[exp, x]` converts sums of rational functions of the form $n + \frac{f_1}{[x-r_1]^p_1} + \frac{f_2}{[x-r_2]^p_2} + \ldots$ to `FCPartialFractionForm[n, {{f1,x-r1,p1},{f2,x-r2,p2}, ...}, x]`.*)


(* ::Text:: *)
(*This facilitates the handling of iterated integrals.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCPartialFractionForm](FCPartialFractionForm.md), [FromFCPartialFractionForm](FromFCPartialFractionForm.md).*)


(* ::Subsection:: *)
(*Examples*)


x/(x+1)

ToFCPartialFractionForm[%,x]


1/(x^2+3)

ToFCPartialFractionForm[%,x]


(-64*(-1+z^2))/(15*(1+z^2+z^4))

ToFCPartialFractionForm[%,z]



