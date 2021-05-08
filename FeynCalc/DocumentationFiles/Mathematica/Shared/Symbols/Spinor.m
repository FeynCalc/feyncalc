 
(* ::Section:: *)
(* Spinor *)
(* ::Text:: *)
(*Spinor[p, m, o] is the head of Dirac spinors. Which of the spinors u, v, $bar{u}$ or $overset{_ }{v}$is understood, depends on the sign of the momentum (p) argument and the relative position in the chain.Spinor[Momentum[p],m] means $bar{u}$ if it stands at the beginning of the chain.Spinor[Momentum[p],m] means $text{u}$ if it stands at the end of the chain.Spinor[-Momentum[p],m] means $bar{v}$ if it stands at the beginning of the chain.Spinor[-Momentum[p],m] means $text{v}$ if it stands at the end of the chain.Spinors of fermions of mass m are normalized to have square $bar{u}$u=2 m and  $overset{_ }{v}$v=-2 m.The optional argument o can be used for additional degrees of freedom. If no optional argument o is supplied, a 1 is subsituted in..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FermionSpinSum, DiracSimplify, SpinorU, SpinorV, SpinorUBar, SpinorVBar, SpinorUBarD, SpinorUD, SpinorVD, SpinorVBarD.*)



(* ::Subsection:: *)
(* Examples *)



Spinor[Momentum[p]]

Spinor[Momentum[p],m]


(* ::Text:: *)
(*FeynCalc uses covariant normalization (as opposed to e.g. the normalization used in Bjorken&Drell).*)


Spinor[Momentum[p],m].Spinor[Momentum[p],m]//DiracSimplify

DiracSimplify[Spinor[-Momentum[p],m].GS[p]]

Spinor[Momentum[p]]//StandardForm

ChangeDimension[Spinor[Momentum[p]],D]//StandardForm

Spinor[Momentum[p],m]//StandardForm


(* ::Text:: *)
(*SmallVariable's are discarded by Spinor.*)


Spinor[Momentum[p],SmallVariable[m]]//StandardForm
