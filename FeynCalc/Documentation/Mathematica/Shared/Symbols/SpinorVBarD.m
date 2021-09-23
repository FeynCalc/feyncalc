(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorVBarD*)


(* ::Text:: *)
(*`SpinorVBarD[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorVBarD[p,m]
FCI[%]//StandardForm


SpinorVBarD[p]
FCI[%]//StandardForm


SpinorVBarD[p] . GSD[p]
DiracEquation[%]
