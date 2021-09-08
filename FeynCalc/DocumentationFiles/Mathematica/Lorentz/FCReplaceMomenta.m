(* ::Package:: *)

(* ::Section:: *)
(*FCReplaceMomenta*)


(* ::Text:: *)
(*`FCReplaceMomenta[exp, rule]`  replaces the given momentum according to the specified replacement rules. Various options can be used to customize the replacement procedure.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCPermuteMomentaRules](FCPermuteMomentaRules.md).*)


(* ::Subsection:: *)
(*Examples*)


amp=(-I)*Spinor[-Momentum[l2],ME,1] . GA[\[Mu]] . Spinor[Momentum[l1],ME,1]*Spinor[Momentum[p1],SMP["m_Q"],1] . GS[Polarization[kp,-I,Transversality->True]] . (GS[kp+p1]+SMP["m_Q"]) . GA[\[Mu]] . Spinor[-Momentum[p2],SMP["m_Q"],1]*FAD[kp+p1+p2,Dimension->4]*FAD[{-l1-l2-p2,SMP["m_Q"]},Dimension->4]*SDF[cq,cqbar]*SMP["e"]^3*SMP["Q_u"]^2


FCReplaceMomenta[amp,{p1->P+1/2 q,p2->P-1/2 q}]
ClearAll[amp]


(* ::Text:: *)
(*Notice that `FCReplaceMomenta` is not suitable for expanding in 4-momenta (soft limits etc.) as it does not check for cases where a particular substitution yields a singularity. For example, the following code obviously returns a nonsensical result*)


FCClearScalarProducts[];
SPD[q]=0;
FCReplaceMomenta[FAD[q+p],{p->0}]
FCClearScalarProducts[];


(* ::Text:: *)
(*`FCReplaceMomenta` equally works with `FCTopology` objects. There it is actually the preferred way to perform momentum shifts. Consider e.g.*)


ex=FCTopology[topo,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}]


(* ::Text:: *)
(*where we want to shift `p_2` to `p_2 + Q`. Doing so naively messes us the topology by invalidating the list of loop momenta*)


ex/.p2->p2+Q
FCLoopValidTopologyQ[%]


(* ::Text:: *)
(*Using `FCReplaceMomenta` we immediately get we want*)


FCReplaceMomenta[ex,{p2->p2+Q}]
