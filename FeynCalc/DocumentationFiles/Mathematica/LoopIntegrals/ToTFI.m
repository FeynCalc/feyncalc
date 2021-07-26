 
(* ::Section:: *)
(*ToTFI*)
(* ::Text:: *)
(*`ToTFI[expr, q1, q2, p]` translates FeynCalc 2-loop self energy type integrals into the `TFI` notation, which can be used as input for the function `TarcerRecurse` from the `TARCER` package. See the `TARCER` documenatation on `TFI` for details on the conventions.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[FromTFI](FromTFI).*)



(* ::Subsection:: *)
(*Examples*)


FAD[q1,q1-p,{q2,M},{q2-p,m},q1-q2]
ToTFI[%,q1,q2,p]
%//StandardForm


SOD[q1] SOD[q2]FAD[q1,q1-p,{q2,M},{q2-p,m},q1-q2]//FCI
