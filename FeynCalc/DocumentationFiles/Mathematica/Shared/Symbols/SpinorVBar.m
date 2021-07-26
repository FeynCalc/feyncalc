(* ::Package:: *)

(* ::Section:: *)
(*SpinorVBar*)


(* ::Text:: *)
(*`SpinorVBar[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorVBar[p,m]
FCI[%]//StandardForm


SpinorVBar[p]
FCI[%]//StandardForm


SpinorVBar[p] . GS[p]
DiracEquation[%]
