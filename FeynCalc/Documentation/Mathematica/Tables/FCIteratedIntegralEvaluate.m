(* ::Package:: *)

 


(* ::Section:: *)
(*FCIteratedIntegralEvaluate*)


(* ::Text:: *)
(*`FCIteratedIntegralEvaluate[ex]` evaluates iterated integrals in ex in terms of multiple polylogarithms.*)


(* ::Text:: *)
(*To that aim the `ex` must contain ration functions (in the `FCPartialFractionForm` notation) and possibly `FCGPL`s wrapped with `FCIteratedIntegral` heads*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCIteratedIntegralSimplify](FCIteratedIntegralSimplify.md), [FCGPL](FCGPL.md).*)


(* ::Subsection:: *)
(*Examples*)


int=FCPartialFractionForm[0,{{{-a+x[2],-1},(1+a+x[3])^(-2)},
{{1+x[2]+x[3],-2},-(1+a+x[3])^(-1)},{{1+x[2]+x[3],-1},-(1+a+x[3])^(-2)}},x[2]]


FCIteratedIntegralEvaluate[FCIteratedIntegral[int,x[2],0,Infinity]]


FCIteratedIntegralEvaluate[FCIteratedIntegral[int,x[2],0,x[2]]]
