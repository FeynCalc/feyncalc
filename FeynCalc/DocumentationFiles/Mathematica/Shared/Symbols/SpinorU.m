 
(* ::Section:: *)
(* SpinorU *)
(* ::Text:: *)
(*SpinorU[p, m] denotes a $u(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorUBar, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorU[p,m]

FCI[%]//StandardForm

SpinorU[p]

FCI[%]//StandardForm

GS[p].SpinorU[p]

DiracEquation[%]
