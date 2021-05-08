 
(* ::Section:: *)
(* SpinorV *)
(* ::Text:: *)
(*SpinorV[p, m] denotes a $v(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorUBar, SpinorU, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorV[p,m]

FCI[%]//StandardForm

SpinorV[p]

FCI[%]//StandardForm

GS[p].SpinorV[p]

DiracEquation[%]
