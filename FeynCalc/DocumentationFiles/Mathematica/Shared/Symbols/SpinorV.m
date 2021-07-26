(* ::Package:: *)

(* ::Section:: *)
(*SpinorV*)


(* ::Text:: *)
(*`SpinorV[p, m]` denotes a $v(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).*)


(* ::Subsection:: *)
(*Examples*)


SpinorV[p,m]
FCI[%]//StandardForm


SpinorV[p]
FCI[%]//StandardForm


GS[p] . SpinorV[p]
DiracEquation[%]
