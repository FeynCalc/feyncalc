 
(* ::Section:: *)
(* SpinorUBarD *)
(* ::Text:: *)
(*SpinorUBarD[p, m] denotes a $\bar{u}(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUD, SpinorVD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorUBarD[p,m]

FCI[%]//StandardForm

SpinorUBarD[p]

FCI[%]//StandardForm

SpinorUBarD[p].GSD[p]

DiracEquation[%]
