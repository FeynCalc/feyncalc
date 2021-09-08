(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorUBarD*)


(* ::Text:: *)
(*`SpinorUBarD[p, m]` denotes a $\bar{u}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBarD[p,m]
FCI[%]//StandardForm


SpinorUBarD[p]
FCI[%]//StandardForm


SpinorUBarD[p] . GSD[p]
DiracEquation[%]
