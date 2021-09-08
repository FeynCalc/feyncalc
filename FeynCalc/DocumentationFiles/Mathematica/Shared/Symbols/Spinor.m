(* ::Package:: *)

 


(* ::Section:: *)
(*Spinor*)


(* ::Text:: *)
(*`Spinor[p, m, o]` is the head of Dirac spinors. Which of the spinors $u$, $v$, $\bar{u}$ or $\bar{v}$ is understood, depends on the sign of the momentum argument `p` and the relative position of `Spinor` in the chain.*)


(* ::Text:: *)
(*- `Spinor[Momentum[p], m]` means $\bar{u}$ if it stands at the beginning of the chain.*)


(* ::Text:: *)
(*- `Spinor[Momentum[p], m]` means $u$ if it stands at the end of the chain.*)


(* ::Text:: *)
(*- `Spinor[-Momentum[p], m]` means $\bar{v}$ if it stands at the beginning of the chain.*)


(* ::Text:: *)
(*- `Spinor[-Momentum[p], m]` means $v$ if it stands at the end of the chain.*)


(* ::Text:: *)
(*Spinors of fermions of mass $m$ are normalized to have $\bar{u} u=2 m$ and  $\bar{v} v=-2 m$.*)


(* ::Text:: *)
(*The optional argument `o` can be used for additional degrees of freedom. If no optional argument `o` is supplied, a `1` is substituted in.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FermionSpinSum](FermionSpinSum.md), [DiracSimplify](DiracSimplify.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorUBar](SpinorUBar.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


Spinor[Momentum[p]]


Spinor[Momentum[p],m]


(* ::Text:: *)
(*FeynCalc uses covariant normalization (as opposed to e.g. the normalization used in Bjorken & Drell).*)


Spinor[Momentum[p],m] . Spinor[Momentum[p],m]//DiracSimplify


DiracSimplify[Spinor[-Momentum[p],m] . GS[p]]


Spinor[Momentum[p]]//StandardForm


ChangeDimension[Spinor[Momentum[p]],D]//StandardForm


Spinor[Momentum[p],m]//StandardForm


(* ::Text:: *)
(*`SmallVariable`s are discarded by `Spinor`.*)


Spinor[Momentum[p],SmallVariable[m]]//StandardForm
