(* ::Package:: *)

 


(* ::Section:: *)
(*TR*)


(* ::Text:: *)
(*`TR[exp]` calculates the Dirac trace of `exp`. Depending on the setting of the option `SUNTrace` also a trace over $SU(N)$ objects is performed.*)


(* ::Text:: *)
(*The Mathematica build-in function `Tr` is overloaded to call `TR` if any of `DiracGamma`, `GA`, `GAD`, `GS` or `GSD` are in the expression.*)


(* ::Text:: *)
(*`Tr[list]` finds the trace of the matrix or tensor list.*)


(* ::Text:: *)
(*`Tr[list, f]` finds a generalized trace, combining terms with f instead of `Plus`.*)


(* ::Text:: *)
(*`Tr[list, f, n]` goes down to level `n` in `list`.*)


(* ::Text:: *)
(*`Tr[expression]` calculates the `DiracTrace`, i.e., `TR[expression]` if any of `DiracGamma`, `GA`, `GAD`, `GS` or `GSD` is present in expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrace](DiracTrace.md), [FermionSpinSum](FermionSpinSum.md), [SUNTrace](SUNTrace.md).*)


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
TR[%,SUNTrace->False,SUNNToCACF->True]
TR[%%,SUNTrace->True,SUNNToCACF->True]


TR[1,SUNTrace->False,SUNNToCACF->True]


TR[1,SUNTrace->True,SUNNToCACF->True]


Tr[ GA[m,n]]
