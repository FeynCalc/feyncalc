(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorV*)


(* ::Text:: *)
(*`SpinorV[p, m]` denotes a $v(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorV[p,m]
FCI[%]//StandardForm


SpinorV[p]
FCI[%]//StandardForm


GS[p] . SpinorV[p]
DiracEquation[%]
