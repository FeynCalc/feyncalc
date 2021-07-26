(* ::Package:: *)

(* ::Section:: *)
(*SpinorUBarD*)


(* ::Text:: *)
(*`SpinorUBarD[p, m]` denotes a $\bar{u}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBarD[p,m]
FCI[%]//StandardForm


SpinorUBarD[p]
FCI[%]//StandardForm


SpinorUBarD[p] . GSD[p]
DiracEquation[%]
