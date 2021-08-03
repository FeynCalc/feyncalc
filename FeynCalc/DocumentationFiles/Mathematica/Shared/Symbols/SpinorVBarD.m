(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorVBarD*)


(* ::Text:: *)
(*`SpinorVBarD[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorVBarD[p,m]
FCI[%]//StandardForm


SpinorVBarD[p]
FCI[%]//StandardForm


SpinorVBarD[p] . GSD[p]
DiracEquation[%]
