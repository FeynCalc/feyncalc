 
(* ::Section:: *)
(*DiracSigmaExplicit*)
(* ::Text:: *)
(*`DiracSigmaExplicit[exp]` inserts in exp for all `DiracSigma` its definition. `DiracSigmaExplict` is also an option of `DiracSimplify`. `DiracSigmaExplict` is also an option of various FeynCalc functions that handle the Dirac algebra.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracGamma](DiracGamma), [DiracSigma](DiracSigma).*)



(* ::Subsection:: *)
(*Examples*)


DiracSigma[GA[\[Alpha]],GA[\[Beta]]]
DiracSigmaExplicit[%]


GSD[p].DiracSigma[GAD[\[Mu]],GAD[\[Nu]]].GSD[q]
DiracSigmaExplicit[%]
