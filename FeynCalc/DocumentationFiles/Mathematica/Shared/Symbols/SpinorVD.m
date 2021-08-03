(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorVD*)


(* ::Text:: *)
(*`SpinorVD[p, m]` denotes a $v(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorVD[p,m]
FCI[%]//StandardForm


SpinorVD[p]
FCI[%]//StandardForm


GSD[p] . SpinorVD[p]
DiracEquation[%]
