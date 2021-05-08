 
(* ::Section:: *)
(* FourLaplacian *)
(* ::Text:: *)
(*FourLaplacian[exp, p, q] is $\partial \left/\partial p_{\mu }\right.$$\partial \left/\partial q_{\mu }\right.$exp..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FourDivergence, RussianTrick.*)



(* ::Subsection:: *)
(* Examples *)



SP[q,q]

FourLaplacian[%,q,q]

SOD[q]^OPEmFAD[q,q-p]//FCI

FourLaplacian[%,q, q]
