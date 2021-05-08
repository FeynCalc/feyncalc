 
(* ::Section:: *)
(* SpinorVBarD *)
(* ::Text:: *)
(*SpinorVBarD[p, m] denotes a $\bar{v}(p,m)$-spinor that depends on the $\text{D}$-dimensional momentum $\text{p}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Spinor, SpinorUBar, SpinorU, SpinorV, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD.*)



(* ::Subsection:: *)
(* Examples *)



SpinorVBarD[p,m]

FCI[%]//StandardForm

SpinorVBarD[p]

FCI[%]//StandardForm

SpinorVBarD[p].GSD[p]

DiracEquation[%]
