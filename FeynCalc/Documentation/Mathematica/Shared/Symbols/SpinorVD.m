(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorVD*)


(* ::Text:: *)
(*`SpinorVD[p, m]` denotes a $v(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorVD[p,m]
FCI[%]//StandardForm


SpinorVD[p]
FCI[%]//StandardForm


GSD[p] . SpinorVD[p]
DiracEquation[%]
