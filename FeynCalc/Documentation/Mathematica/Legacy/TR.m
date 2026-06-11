(* ::Package:: *)

 


(* ::Section:: *)
(*TR*)


(* ::Text:: *)
(*`TR[exp]` calculates the Dirac trace of `exp`.*)


(* ::Text:: *)
(*If the option `SUNSimplify` is set to `True` (default), $SU(N)$ algebra is simplified as well.*)


(* ::Text:: *)
(*Notice that `TR` is a legacy function that should not be used in new codes. Instead, you can wrap your string Dirac matrices with `DiracTrace` and subsequently apply `DiracSimplify` to calculate the trace.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrace](DiracTrace.md), [SUNSimplify](SUNSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


GA[\[Mu],\[Nu]]

TR[%]


TR[(GSD[p]+m) . GAD[\[Mu]] . (GSD[q]-m) . GAD[\[Nu]]]


TR[GA[\[Mu],\[Nu],\[Rho],\[Sigma],5]]


TR[GS[p,q,r,s]]


TR[(GS[p]+m) . GA[\[Mu]] . (GS[q]+m) . GA[\[Mu]],Factoring->True]


TR[GA[\[Alpha],\[Beta]],FCE->True]


GA[\[Mu],\[Nu]] SUNT[b] . SUNT[c] SUNDelta[c,b]

TR[%]



