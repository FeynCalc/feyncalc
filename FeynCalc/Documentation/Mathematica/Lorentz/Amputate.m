(* ::Package:: *)

 


(* ::Section:: *)
(*Amputate*)


(* ::Text:: *)
(*`Amputate[exp, q1, q2, ...]` amputates `Eps` and `DiracGamma`. `Amputate[exp,q1,q2,Pair->{p}]` amputates also `p.q1` and `p.q2`; `Pair->All` amputates all except `OPEDelta`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [DiracSimplify](DiracSimplify.md), [GS](GS.md), [DiracTrick](DiracTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


GS[p] . GS[q]


Amputate[%,q]
