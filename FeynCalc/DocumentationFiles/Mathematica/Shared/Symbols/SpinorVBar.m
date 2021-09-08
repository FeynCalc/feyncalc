(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorVBar*)


(* ::Text:: *)
(*`SpinorVBar[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorVBar[p,m]
FCI[%]//StandardForm


SpinorVBar[p]
FCI[%]//StandardForm


SpinorVBar[p] . GS[p]
DiracEquation[%]
