 
(* ::Section:: *)
(* DiracSigma *)
(* ::Text:: *)
(*DiracSigma[a, b] stands for i/2*(a . b - b . a) in 4 dimensions. a and b must have Head DiracGamma, GA or GS. Only antisymmetry is implemented..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracSigmaExplicit.*)



(* ::Subsection:: *)
(* Examples *)



DiracSigma[GA[\[Alpha]],GA[\[Beta]]]

DiracSigmaExplicit[%]

DiracSigma[GA[\[Beta]],GA[\[Alpha]]]

DiracSigma[GS[p],GS[q]]

DiracSigmaExplicit[%]


(* ::Text:: *)
(*The antisymmetry propery is built-in*)


DiracSigma[GA[\[Alpha]],GA[\[Alpha]]]
