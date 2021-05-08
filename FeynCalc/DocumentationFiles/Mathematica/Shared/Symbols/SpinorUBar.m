 
(* ::Section:: *)
(* SpinorUBar *)
(* ::Text:: *)
(*SpinorUBar[p, m] denotes a $\bar{u}(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorUBar[p,m]

FCI[%]//StandardForm

SpinorUBar[p]

FCI[%]//StandardForm

SpinorUBar[p].GS[p]

DiracEquation[%]
