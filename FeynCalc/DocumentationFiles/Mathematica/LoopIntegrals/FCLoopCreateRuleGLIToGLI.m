(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopCreateRuleGLIToGLI*)


(* ::Text:: *)
(*`FCLoopCreateRuleGLIToGLI[topology1, topology2]` creates a GLI replacement rule assuming that the `topology2` is a subtopology of `topology1`. Both topologies must be given as `FCTopology` objects.*)


(* ::Text:: *)
(*It is also possible to use `FCLoopCreateRuleGLIToGLI[topo1, {subtopo1, subtopo2, ...}]` provided that `{subtopo1, subtopo2, ...}` are subtopologies of `topo1` that were obtained by removing some propagators from `topo1` and not performing any loop momentum shifts afterwards.*)


(* ::Text:: *)
(*Furthermore, when working with lists of topologies one can write `FCLoopCreateRuleGLIToGLI[{topo1, topo2, ...}, {{subtopo11, subtopo12, ...}, {subtopo21, subtopo22, ...}, ..}]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCFindTopologies](FCFindTopologies.md), [FCFindTopologyMappings](FCFindTopologyMappings.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopCreateRuleGLIToGLI[FCTopology[topo1,{SFAD[p]}],FCTopology[topo2,{SFAD[p]}]]


FCLoopCreateRuleGLIToGLI[FCTopology[topo1,{SFAD[p],SFAD[q]}],
FCTopology[topo2,{SFAD[p]}]]


FCLoopCreateRuleGLIToGLI[FCTopology[topo1,{SFAD[p],SFAD[q]}],
FCTopology[topo2,{SFAD[q],SFAD[p]}]]


FCLoopCreateRuleGLIToGLI[FCTopology[topo1,{SFAD[r],SFAD[p],SFAD[q]}],
FCTopology[topo2,{SFAD[p]}]]


FCLoopCreateRuleGLIToGLI[FCTopology["tmpTopo4",
{SFAD[{{0,(k1+k2) . nb},{0,1},1}],SFAD[{{0,(k1-k3) . n},{0,1},1}],
SFAD[{{0,n . (-k1-k2+q)},{0,1},1}],SFAD[{{0,nb . (-k1+k3+q)},{0,1},1}],
SFAD[{{-k1,0},{0,1},1}],SFAD[{{k2,0},{0,1},1}],SFAD[{{k1+k2,0},{0,1},1}],
SFAD[{{-k3,0},{0,1},1}],SFAD[{{-k1+k3,0},{0,1},1}],
SFAD[{{k1-k3-q,0},{0,1},1}],SFAD[{{k1+k2-k3-q,0},{0,1},1}],
SFAD[{{-k1-k2+q,0},{0,1},1}]}],

FCTopology["tmpTopo18",{SFAD[{{0,(k1+k2) . nb},{0,1},1}],
SFAD[{{0,n . (-k1-k2+q)},{0,1},1}],SFAD[{{0,nb . (-k1+k3+q)},{0,1},1}],
SFAD[{{-k1,0},{0,1},1}],SFAD[{{k2,0},{0,1},1}],
SFAD[{{k1+k2,0},{0,1},1}],SFAD[{{-k3,0},{0,1},1}],
SFAD[{{-k1+k3,0},{0,1},1}],SFAD[{{k1-k3-q,0},{0,1},1}],
SFAD[{{k1+k2-k3-q,0},{0,1},1}],SFAD[{{-k1-k2+q,0},{0,1},1}]}]]


FCLoopIntegralToGraph[FCTopology["tad2l",{FAD[{p1,m1}],FAD[{p2,m2}],FAD[{p1-p2,m3}]},{p1,p2},{},{},{}]]


FCLoopCreateRuleGLIToGLI[
{FCTopology["prop2l",{FAD[{p1,m1}],FAD[{p2,m2}],FAD[{p1-q,m3}],FAD[{p1-q,m4}],FAD[{p1-p2,m5}]},{p1,p2},{q},{},{}],
FCTopology["tad2l",{FAD[{p1,m1}],FAD[{p2,m2}],FAD[{p1-p2,m3}]},{p1,p2},{},{},{}]},{
{
FCTopology["prop2lX1",{FAD[{p2,m2}],FAD[{p1-q,m3}],FAD[{p1-q,m4}],FAD[{p1-p2,m5}]},{p1,p2},{q},{},{}],
FCTopology["prop2lX5",{FAD[{p1,m1}],FAD[{p2,m2}],FAD[{p1-q,m3}],FAD[{p1-q,m4}]},{p1,p2},{q},{},{}]
},
{
FCTopology["tad2lX2",{FAD[{p1,m1}],FAD[{p1-p2,m3}]},{p1,p2},{},{},{}],
FCTopology["tad2lX3",{FAD[{p1,m1}],FAD[{p2,m2}]},{p1,p2},{},{},{}]
}
}]
