(* ::Package:: *)

(* ::Section:: *)
(*FCLoopToPakForm*)


(* ::Text:: *)
(*`FCLoopToPakForm[int, {p1, p2, ...}]` determines a canonical $UF$-based representation for the scalar multi-loop integral `int` that depend on the loop momenta `p1, p2, ...` using the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868).*)


(* ::Text:: *)
(*The current implementation is based on the `FindEquivalents` function from FIRE 6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808). `FCLoopToPakForm` is a backend function used in `FCLoopPakScalelessQ`, `FCLoopFindIntegralMappings`, `FCLoopFindTopologyMappings` etc.*)


(* ::Text:: *)
(*It is also possible to invoke the function as `FCLoopToPakForm[GLI[...], FCTopology[...]]` or FCLoopToPakForm[FCTopology[...]]. Notice that in this case the value of the option `FinalSubstitutions` is ignored, as replacement rules will be extracted directly from the definition of the topology.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakScalelessQ](FCLoopPakScalelessQ.md), [FCLoopScalelessQ](FCLoopScalelessQ.md), [FCLoopFindIntegralMappings](FCLoopFindIntegralMappings.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopToPakForm[FAD[p1,{p3,m1},{p1-p4,m1},p1+q1,p1+q1,p3+q1,p1-p3-p4],
{p1,p3,p4},Names->x,Head->ph,Power->pow]


topo1=FCTopology["prop2Lv1",{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[p1-q],SFAD[p2-q],SFAD[{p1-p2,m3^2}]},{p1,p2},{Q},{},{}]
topo2=FCTopology["prop2Lv2",{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[{p1-q,M^2}],SFAD[{p2-q,M^2}],SFAD[p1-p2]},{p1,p2},{Q},{},{}]


FCLoopToPakForm[topo1,Names->x,Head->ph,Power->pow]


FCLoopToPakForm[{GLI["prop2Lv1",{1,1,1,1,0}],GLI["prop2Lv2",{1,1,0,0,1}]},{topo1,topo2},Names->x,Head->ph,Power->pow]


(* ::Text:: *)
(*Products of `GLI`s are also supported.*)


FCLoopToPakForm[{GLI["prop2Lv1",{1,1,0,0,0}]^2},{topo1,topo2},Names->x,Head->ph,Power->pow]
