(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopApplyTopologyMappings*)


(* ::Text:: *)
(*`FCLoopApplyTopologyMappings[expr, {mappings, topos}]` applies mappings between topologies obtained using `FCLoopFindTopologyMappings` to the output of `FCLoopFindTopologies` denoted as `expr`. The argument `topos` denotes the final set of topologies present in the expression.*)


(* ::Text:: *)
(*Instead of `{mappings, topos}` one can directly use the output `FCLoopFindTopologyMappings`.*)


(* ::Text:: *)
(*By default the function will attempt to rewrite all the occurring loop integrals as `GLI`s. If you just want to apply the mappings without touching the remaining scalar products,  set the option `FCLoopCreateRulesToGLI` to `False`. Even when all scalar products depending on loop momenta are rewritten as `GLI`s, you can still suppress the step of multiplying out products*)
(*of `GLI`s by setting the option `GLIMultiply` to `False`.*)


(* ::Text:: *)
(*If there are no mappings to apply and you merely want to have the output that only involves `GLI`s, you can call the function as `FCLoopApplyTopologyMappings[expr, {{}, topos}]` or just `FCLoopApplyTopologyMappings[expr, topos]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md).*)


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
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],{p1->-p1-p3+Q,p2->-p2-p3+Q,p3->p3},
GLI[fctopology3,{n1_,n7_,n8_,n5_,n6_,n4_,n2_,n3_,n9_}]:>
GLI[fctopology1,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]},

{FCTopology[fctopology4,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],{p1->-p2+Q,p2->-p1+Q,p3->-p3},
GLI[fctopology4,{n1_,n6_,n5_,n8_,n7_,n3_,n2_,n4_,n9_}]:>
GLI[fctopology1,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]},

{FCTopology[fctopology5,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],
SFAD[{{p1+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],
SFAD[{{p1+p2-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],{p1->p2,p2->p1,p3->p3},
GLI[fctopology5,{n1_,n3_,n2_,n4_,n6_,n5_,n7_,n8_,n9_}]:>
GLI[fctopology2,{n1,n2,n3,n4,n5,n6,n7,n8,n9}]}}


(* ::Text:: *)
(*These are the two topologies onto which everything is mapped*)


finalTopos={
FCTopology[fctopology1,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p3-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}],
FCTopology[fctopology2,{SFAD[{{p3,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],SFAD[{{p1,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],SFAD[{{p2-Q,0},{0,1},1}],
SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],SFAD[{{p1+p2-Q,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}]},{p1,p2,p3},{Q},{},{}]}


(* ::Text:: *)
(*`FCLoopApplyTopologyMappings`  applies the given mappings to the expression creating an output that is ready to be processed further*)


FCLoopApplyTopologyMappings[ex,{mappings,finalTopos},Head->gliProduct,FCVerbose->0]


(* ::Text:: *)
(*This just applies the mappings without any further simplifications*)


FCLoopApplyTopologyMappings[ex,{mappings,finalTopos},Head->gliProduct,FCLoopCreateRulesToGLI->False]


(* ::Text:: *)
(*This applies the mappings and eliminates the numerators but still keeps products of `GLI`s in the expression*)


FCLoopApplyTopologyMappings[ex,{mappings,finalTopos},Head->gliProduct,FCLoopCreateRulesToGLI->True,GLIMultiply->False]
