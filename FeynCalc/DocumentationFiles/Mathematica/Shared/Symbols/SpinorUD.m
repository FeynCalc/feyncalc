(* ::Package:: *)

(* ::Section:: *)
(*SpinorUD*)


(* ::Text:: *)
(*`SpinorUD[p, m]` denotes a $u(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUD[p,m]
FCI[%]//StandardForm


SpinorUD[p]
FCI[%]//StandardForm


GSD[p] . SpinorUD[p]
DiracEquation[%]
