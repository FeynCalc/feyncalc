(* ::Package:: *)

 


(* ::Section:: *)
(*EpsChisholm*)


(* ::Text:: *)
(*`EpsChisholm[exp]` applies the Chisholm identity to a Dirac matrix contracted with a Levi-Civita tensor.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Chisholm](Chisholm.md), [Eps](Eps.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


LC[\[Mu],\[Nu],\[Rho],\[Sigma]]GA[\[Sigma],5]
EpsChisholm[%]


(* ::Text:: *)
(*This reproduces the identities given in the Appendix A of (arXiv:2111.05153)[https://arxiv.org/abs/2111.05153]*)


LC[\[Alpha],\[Nu],\[Beta],\[Rho]]FV[Subscript[p, 1],\[Beta]]SpinorUBar[Subscript[p, 2],SMP["m_s"]] . GA[\[Alpha],7] . SpinorV[Subscript[p, 1],SMP["m_d"]]
%//EpsChisholm//DiracSimplify//Contract


LC[\[Alpha],\[Nu],\[Beta],\[Rho]]FV[Subscript[p, 2],\[Beta]]SpinorUBar[Subscript[p, 2],SMP["m_s"]] . GA[\[Alpha],7] . SpinorV[Subscript[p, 1],SMP["m_d"]]
%//EpsChisholm//DiracSimplify//Contract


LC[\[Alpha],\[Nu],\[Gamma],\[Rho]]FV[Subscript[p, 3],\[Gamma]]SpinorUBar[Subscript[p, 3],SMP["m_s"]] . GA[\[Nu],7] . SpinorV[Subscript[p, 4],SMP["m_d"]]
%//EpsChisholm//DiracSimplify//Contract


LC[\[Beta],\[Gamma],\[Mu],\[Nu]]FV[Subscript[p, 2],\[Gamma]]SpinorUBar[Subscript[p, 3],SMP["m_s"]] . GA[\[Beta],7] . SpinorV[Subscript[p, 4],SMP["m_d"]]
%//EpsChisholm//DiracSimplify//Contract



