(* ::Package:: *)

 


(* ::Section:: *)
(*FCRerouteMomenta*)


(* ::Text:: *)
(*`FCRerouteMomenta[exp, {p1, p2, ...}, {k1, k2, ...}]`  changes the routing of the momenta by exploiting the 4-momentum conservation law $p_1+p_2+ \ldots = k_1+k_2+ \ldots$.*)


(* ::Text:: *)
(*The main aim of this function is to simplify the input expression by replacing simple linear combinations of the external momenta with shorter expressions.*)


(* ::Text:: *)
(*For example, in a process $a(p_1) + b(p_2) -> c(k_1)+ d(k_2)+ e(k_3)$, the combination $k_1+k_2-p_2$ can be replaced with the shorter expression $p_1-k_3$.*)


(* ::Text:: *)
(*The replacements are applied using the `FeynCalcExternal` form of the expression. Ideally, this function should be used directly on the output of a diagram generator such as FeynArts or QGRAF.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Reroute momenta according to the momentum conservation relation $l_1+l_2=p_1+p_2+k_p$.*)


exp=(-I)*Spinor[-Momentum[l2],ME,1] . GA[\[Mu]] . Spinor[Momentum[l1],ME,
1]*Spinor[Momentum[p1],SMP["m_Q"],1] . GS[Polarization[kp,-I,
Transversality->True]] . (GS[kp+p1]+SMP["m_Q"]) . GA[\[Mu]] . Spinor[-Momentum[p2],
SMP["m_Q"],1]*FAD[kp+p1+p2,Dimension->4]*FAD[{-l1-l2-p2,SMP["m_Q"]},
Dimension->4]*SDF[cq,cqbar]*SMP["e"]^3*SMP["Q_u"]^2


FCRerouteMomenta[exp,{l1,l2},{p1,p2,kp}]
