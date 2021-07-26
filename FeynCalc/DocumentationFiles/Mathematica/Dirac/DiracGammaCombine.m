 
(* ::Section:: *)
(*DiracGammaCombine*)
(* ::Text:: *)
(*`DiracGammaCombine[exp]` is (nearly) the inverse operation to `DiracGammaExpand`.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracGamma](DiracGamma), [DiracGammaExpand](DiracGammaExpand), [DiracSimplify](DiracSimplify), [DiracTrick](DiracTrick).*)



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
