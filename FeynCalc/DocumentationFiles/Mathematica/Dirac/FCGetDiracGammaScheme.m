 
(* ::Section:: *)
(*FCGetDiracGammaScheme*)
(* ::Text:: *)
(*`FCGetDiracGammaScheme[]` shows the currently used scheme for handling Dirac matrices in $D$ dimensions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCSetDiracGammaScheme](FCSetDiracGammaScheme.md), [DiracTrace](DiracTrace.md).*)



(* ::Subsection:: *)
(*Examples*)


FCSetDiracGammaScheme["BMHV"]
FCGetDiracGammaScheme[]
%//FullForm


FCSetDiracGammaScheme["NDR"]
FCGetDiracGammaScheme[]
%//FullForm
