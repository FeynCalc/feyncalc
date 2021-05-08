 
(* ::Section:: *)
(* FCRerouteMomenta *)
(* ::Text:: *)
(*FCRerouteMomenta[exp, {p1, p2, ...}, {k1, k2, ...}]  changes the routing of the momenta by exploiting the 4-momentum conservation law p1+p2+... = k1+k2+... The main aim of this function is to simplify the input expression by replacing simple linear combinations of the external momenta with shorter expressions. For example, in a process p1+p2 -> k1+k2+k3, the combination k1+k2-p2 can be replaced with the shorter expression p1-k3. The replacements are applied using the FeynCalcExternal form of the expression. Ideally, this function should be used directly on the output of a diagram generator such as FeynArts or QGRAF..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Reroute momenta according to the momentum conservation relation $text{l1}+text{l2}=text{p1}+text{p2}+text{kp}$.*)



(* ::Subsection:: *)
(* Examples *)



(-I)*Spinor[-Momentum[l2],ME,1].GA[\[Mu]].Spinor[Momentum[l1],ME,1]*Spinor[Momentum[p1],SMP["m_Q"],1].GS[Polarization[kp,-I,Transversality->True]].(GS[kp+p1]+SMP["m_Q"]).GA[\[Mu]].Spinor[-Momentum[p2],SMP["m_Q"],1]*FAD[kp+p1+p2,Dimension->4]*FAD[{-l1-l2-p2,SMP["m_Q"]},Dimension->4]*SDF[cq,cqbar]*SMP["e"]^3*SMP["Q_u"]^2

FCRerouteMomenta[%,{l1,l2},{p1,p2,kp}]
