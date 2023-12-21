(* ::Package:: *)

 


(* ::Section:: *)
(*FCDiffEqSolve*)


(* ::Text:: *)
(*`FCDiffEqSolve[mat, var, eps, n]` constructs a solution for a single-variable differential equation $G' = \varepsilon \mathcal{B} G$ in the canonical form, where `mat` is $B$, `var` is the variable w.r.t. which $G$ was differentiated and `n` is the required order in `eps`.*)


(* ::Text:: *)
(*The output consists of iterated integrals written in terms of `FCIteratedIntegral` objects.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCDiffEqChangeVariables](FCDiffEqChangeVariables.md)*)


(* ::Subsection:: *)
(*Examples*)


mat={{-2/x, 0, 0}, {0, 0, 0}, {-x^(-1), 3/x, -2/x}}


FCDiffEqSolve[mat,x,ep,1]
