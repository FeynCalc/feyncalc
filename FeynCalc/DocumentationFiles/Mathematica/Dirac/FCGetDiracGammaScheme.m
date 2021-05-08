 
(* ::Section:: *)
(* FCGetDiracGammaScheme *)
(* ::Text:: *)
(*`FCGetDiracGammaScheme[]` shows the currently used scheme for handling Dirac matrices in $D$ dimensions.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FCSetDiracGammaScheme, DiracTrace.*)



(* ::Subsection:: *)
(* Examples *)


FCSetDiracGammaScheme["BMHV"]
FCGetDiracGammaScheme[]
%//FullForm


FCSetDiracGammaScheme["NDR"]
FCGetDiracGammaScheme[]
%//FullForm
