(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorUBar*)


(* ::Text:: *)
(*`SpinorUBar[p, m]` denotes a $\bar{u}(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p,m]
FCI[%]//StandardForm


SpinorUBar[p]
FCI[%]//StandardForm


SpinorUBar[p] . GS[p]
DiracEquation[%]
