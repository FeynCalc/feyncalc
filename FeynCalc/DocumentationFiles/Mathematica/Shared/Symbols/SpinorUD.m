(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorUD*)


(* ::Text:: *)
(*`SpinorUD[p, m]` denotes a $u(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUD[p,m]
FCI[%]//StandardForm


SpinorUD[p]
FCI[%]//StandardForm


GSD[p] . SpinorUD[p]
DiracEquation[%]
