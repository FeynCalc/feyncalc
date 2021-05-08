 
(* ::Section:: *)
(* SpinorVBar *)
(* ::Text:: *)
(*SpinorVBar[p, m] denotes a $\bar{v}(p,m)$-spinor that depends on the $4$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorUBar, SpinorU, SpinorV, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorVBar[p,m]

FCI[%]//StandardForm

SpinorVBar[p]

FCI[%]//StandardForm

SpinorVBar[p].GS[p]

DiracEquation[%]
