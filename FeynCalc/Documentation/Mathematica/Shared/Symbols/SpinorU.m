(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorU*)


(* ::Text:: *)
(*`SpinorU[p, m]` denotes a $u(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorU[p,m]
FCI[%]//StandardForm


SpinorU[p]
FCI[%]//StandardForm


GS[p] . SpinorU[p]
DiracEquation[%]
