 
(* ::Section:: *)
(* SpinorVD *)
(* ::Text:: *)
(*SpinorVD[p, m] denotes a $v(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorVD[p,m]

FCI[%]//StandardForm

SpinorVD[p]

FCI[%]//StandardForm

GSD[p].SpinorVD[p]

DiracEquation[%]
