(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopCreateRuleGLIToGLI*)


(* ::Text:: *)
(*`FCLoopCreateRuleGLIToGLI[topology1, topology2]` creates a GLI replacement rule assuming that the `topology2` is a subtopology of `topology1`. Both topologies must be given as `FCTopology` objects.*)


(* ::Text:: *)
(*It is also possible to use `FCLoopCreateRuleGLIToGLI[topo1, {subtopo1, subtopo2, ...}]` provided that `{subtopo1, subtopo2, ...}` are subtopologies of `topo1` that were obtained by removing some propagators from `topo1` and not performing any loop momentum shifts afterwards.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI), [FCFindTopologies](FCFindTopologies), [FCFindTopologyMappings](FCFindTopologyMappings).*)


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
