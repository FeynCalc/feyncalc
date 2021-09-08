 
(* ::Section:: *)
(*DiracGammaCombine*)
(* ::Text:: *)
(*`DiracGammaCombine[exp]` is (nearly) the inverse operation to `DiracGammaExpand`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracGammaExpand](DiracGammaExpand.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).*)



(* ::Subsection:: *)
(*Examples*)



GS[p] + GS[q]
DiracGammaCombine[%]
StandardForm[%]


2 GSD[p] - 3 GSD[q]
DiracGammaCombine[%]
StandardForm[%]


DiracGammaCombine[2 GSD[p] - 3 GSD[q]]
DiracGammaExpand[%]
