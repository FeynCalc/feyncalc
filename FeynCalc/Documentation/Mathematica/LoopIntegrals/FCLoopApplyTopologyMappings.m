(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopApplyTopologyMappings*)


(* ::Text:: *)
(*`FCLoopApplyTopologyMappings[expr, mappings]` applies mappings between topologies obtained using `FCFindTopologyMappings` to the output of `FCFindTopologies` denoted as `expr`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCFindTopologies](FCFindTopologies.md), [FCFindTopologyMappings](FCFindTopologyMappings.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This is a trial expression representing some loop amplitude that has already been processed using `FCFindTopologies`*)


ex=gliProduct[cc6*SPD[p1,p1],GLI[fctopology1,{1,1,2,1,1,1,1,1,1}]]+
gliProduct[cc2*SPD[p1,p2],GLI[fctopology2,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc4*SPD[p1,p2],GLI[fctopology4,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc1*SPD[p1,Q],GLI[fctopology1,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc3*SPD[p2,p2],GLI[fctopology3,{1,1,1,1,1,1,1,1,1}]]+
gliProduct[cc5*SPD[p2,Q],GLI[fctopology5,{1,1,1,1,1,1,1,1,1}]]


(* ::Text:: *)
(*These mapping rules describe how the 3 topologies "fctopology3", "fctopology4" and "fctopology5" are mapped to the topologies "fctopology1" and "fctopology2"*)


mappings={
{FCTopology[fctopology3,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]}],{p1->-p1-p3+Q,p2->-p2-p3+Q,p3->p3},
GLI[fctopology3,{n1_,n7_,n8_,n5_,n6_,n4_,n2_,n3_,n9_}]:>GLI[fctopology1,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]},

{FCTopology[fctopology4,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]}],{p1->-p2+Q,p2->-p1+Q,p3->-p3},
GLI[fctopology4,{n1_,n6_,n5_,n8_,n7_,n3_,n2_,n4_,n9_}]:>GLI[fctopology1,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]},

{FCTopology[fctopology5,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]}],{p1->p2,p2->p1,p3->p3},
GLI[fctopology5,{n1_,n3_,n2_,n4_,n6_,n5_,n7_,n8_,n9_}]:>GLI[fctopology2,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]}}


(* ::Text:: *)
(*`FCLoopApplyTopologyMappings`  applies the given mappings to the expression creating an output that is ready to be processed further.*)


FCLoopApplyTopologyMappings[ex,mappings,Head->gliProduct]
