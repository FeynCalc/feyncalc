(* ::Package:: *)

 


(* ::Section:: *)
(*FCIteratedIntegralSimplify*)


(* ::Text:: *)
(*`FCIteratedIntegralSimplify[ex]` uses linearity to simplify nested products and linear combinations of `FCIteratedIntegral`s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCIteratedIntegral](FCIteratedIntegral.md), [FCGPL](FCGPL.md).*)


(* ::Subsection:: *)
(*Examples*)


int=C[1,0]+Epsilon*(C[1,1]+FCIteratedIntegral[C[1,0]*FCPartialFractionForm[0,{{{x,-1},-2}},x],x])+
Epsilon^2*(C[1,2]+FCIteratedIntegral[(C[1,1]+FCIteratedIntegral[C[1,0]*FCPartialFractionForm[0,
{{{x,-1},-2}},x],x])*FCPartialFractionForm[0,{{{x,-1},-2}},x],x])+
Epsilon^3*(C[1,3]+FCIteratedIntegral[(C[1,2]+FCIteratedIntegral[(C[1,1]+
FCIteratedIntegral[C[1,0]*FCPartialFractionForm[0,{{{x,-1},-2}},x],x])*FCPartialFractionForm[0,
{{{x,-1},-2}},x],x])*FCPartialFractionForm[0,{{{x,-1},-2}},x],x])


FCIteratedIntegralSimplify[int]
