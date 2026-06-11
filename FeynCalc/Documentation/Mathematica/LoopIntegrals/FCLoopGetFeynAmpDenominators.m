(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGetFeynAmpDenominators*)


(* ::Text:: *)
(*`FCLoopGetFeynAmpDenominators[expr, {k1,k2,...}, head]` extracts all single propagator denominators present in the expression that depend on the loop momenta `k1, k2, ...`.*)


(* ::Text:: *)
(*The function returns a list of two elements.  The first one contains the original expression with selected denominators wrapped with `head`. The second one is the list of relevant denominators*)


(* ::Text:: *)
(*Setting the option `"Massless"` to `True`will select only massless denominators.*)


(* ::Text:: *)
(*The option `Momentum` specifies the dependency on external momenta. When set to a list of momenta, relevant propagators will be selected irrespective of being massless or massive.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine.md).*)


(* ::Subsection:: *)
(*Examples*)


amp=Get@FileNameJoin[{$FeynCalcDirectory,"Documentation",
"Examples","Amplitudes","Q-Q-massless-2L.m"}];


FCReloadFunctionFromFile[FCLoopGetFeynAmpDenominators]


(* ::Text:: *)
(*All denominators depending on k1, k2*)


FCLoopGetFeynAmpDenominators[amp,{k1,k2},denHead]//Last


(* ::Text:: *)
(*All denominators depending on k1, k2 and the external momentum p*)


FCLoopGetFeynAmpDenominators[amp,{k1,k2},denHead,Momentum->{p}]//Last


(* ::Text:: *)
(*All denominators depending on k1, k2 and the external momentum p as well as massless denominators*)


FCLoopGetFeynAmpDenominators[amp,{k1,k2},denHead,Momentum->{p},"Massless"->True]//Last
