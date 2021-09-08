(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorUBar*)


(* ::Text:: *)
(*`SpinorUBar[p, m]` denotes a $\bar{u}(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p,m]
FCI[%]//StandardForm


SpinorUBar[p]
FCI[%]//StandardForm


SpinorUBar[p] . GS[p]
DiracEquation[%]
