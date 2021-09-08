(* ::Package:: *)

 


(* ::Section:: *)
(*FCApart*)


(* ::Text:: *)
(*`FCApart[expr, {q1, q2, ...}]` is an internal function that partial fractions a loop integral (that depends on `q1`,`q2`, ...) into integrals that contain only linearly independent propagators. The algorithm is largely based on [arXiv:1204.2314](https://arxiv.org/abs/1204.2314) by F.Feng. `FCApart` is meant to be applied to single loop integrals only. If you need to perform partial fractioning on an expression that contains multiple loop integrals, use `ApartFF`.*)


(* ::Text:: *)
(*There is actually no reason, why one would want to apply `FCApart` instead of `ApartFF`, except for cases, where `FCApart` is called from a different package that interacts with FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ApartFF](ApartFF.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


SPD[q,q]FAD[{q,m}]
FCApart[%,{q}]


SPD[q,p]SPD[q,r]FAD[{q},{q-p},{q-r}]
FCApart[%,{q}]


SPD[p,q1]SPD[p,q2]^2FAD[{q1,m},{q2,m},q1-p,q2-p,q1-q2]
FCApart[%,{q1,q2}]
