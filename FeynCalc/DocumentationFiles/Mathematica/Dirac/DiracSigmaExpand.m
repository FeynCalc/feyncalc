(* ::Package:: *)

 


(* ::Section:: *)
(*DiracSigmaExpand*)


(* ::Text:: *)
(*`DiracSigmaExpand[exp]` applies linearity to the arguments of `DiracSigma`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracSigma](DiracSigma.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracSigma[GSD[p]+GSD[q],GSD[r]]
%//DiracSigmaExpand
%//FCE//StandardForm


(* ::Text:: *)
(*Notice that DiracSigmaExpand does not expand Dirac matrices contracted to linear combinations of $4$-vectors by default.*)


DiracSigma[GSD[p+q],GSD[r]]
DiracSigmaExpand[%]


(* ::Text:: *)
(*If such expansions are required, use the option `DiracGammaExpand`.*)


DiracSigmaExpand[DiracSigma[GSD[p+q],GSD[r]],DiracGammaExpand->True]


(* ::Text:: *)
(*The option Momentum allows us to perform more fine-grained expansions of `DiracSigma`.*)


DiracSigma[GSD[p],GSD[r]+GSD[t]]+DiracSigma[GSD[l]+GSD[n],GSD[p]]
DiracSigmaExpand[%,Momentum->{r}]
