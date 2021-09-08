(* ::Package:: *)

 


(* ::Section:: *)
(*DiracGammaExpand*)


(* ::Text:: *)
(*`DiracGammaExpand[exp]` expands Dirac matrices contracted to linear combinations of $4$-vectors. All `DiracGamma[Momentum[a+b+ ...]]` will be expanded to `DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + DiracGamma[Momentum[...]]` .*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracGammaCombine](DiracGammaCombine.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


GS[q] . GS[p-q]
DiracGammaExpand[%]
StandardForm[%]


(* ::Text:: *)
(*`DiracGammaExpand` rewrites $\gamma^{\mu } (p-q)_{\mu }$ as $\gamma^{mu } p_{mu } - \gamma^{\mu } q_{\mu }$.*)


(* ::Text:: *)
(*The inverse operation is `DiracGammaCombine`.*)


GS[q] . (GS[p]-GS[q])
DiracGammaCombine[%]
StandardForm[%]


(* ::Text:: *)
(*It is possible to perform the expansions only on Dirac matrices contracted with particular momenta.*)


c1 GAD[\[Mu]] . (GSD[p1+p2]+m) . GAD[\[Nu]]+ c2 GAD[\[Mu]] . (GSD[q1+q2]+m) . GAD[\[Nu]]
DiracGammaExpand[%,Momentum->{q1}]


(* ::Text:: *)
(*If the input expression contains `DiracSigma`,  `DiracGammaExpand` will expand Feynman slashes inside `DiracSigma` and call `DiracSigmaExpand`.*)


DiracSigma[GSD[p+q],GSD[r]]
DiracGammaExpand[%]


(* ::Text:: *)
(*The call to `DiracSigmaExpand` can be inhibited by disabling the corresponding option.*)


DiracGammaExpand[DiracSigma[GSD[p+q],GSD[r]],DiracSigmaExpand->False]


(* ::Text:: *)
(*Use `DiracSimplify` for noncommutative expansions with the corresponding simplifications.*)


DiracSimplify[GS[q] . (GS[p-q])]


(* ::Text:: *)
(*If simplifications are not required, you may also combine `DiracGammaExpand` with `DotSimplify`.*)


DotSimplify[DiracGammaExpand[GS[q] . (GS[p-q])]]
