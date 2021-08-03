(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorU*)


(* ::Text:: *)
(*`SpinorU[p, m]` denotes a $u(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorU[p,m]
FCI[%]//StandardForm


SpinorU[p]
FCI[%]//StandardForm


GS[p] . SpinorU[p]
DiracEquation[%]
