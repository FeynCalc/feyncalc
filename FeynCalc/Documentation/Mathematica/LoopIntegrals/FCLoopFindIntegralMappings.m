(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindIntegralMappings*)


(* ::Text:: *)
(*`FCLoopFindIntegralMappings[{int1, int2, ...}, {p1, p2, ...}]` finds mappings between scalar multiloop integrals `int1, int2, ...` that depend on the loop momenta `p1, p2, ...` using the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868).*)


(* ::Text:: *)
(*The current implementation is based on the `FindEquivalents` function from FIRE 6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808)*)


(* ::Text:: *)
(*It is also possible to invoke the function as `FCLoopFindIntegralMappings[{GLI[...], ...}, {FCTopology[...], ...}] or FCLoopFindIntegralMappings[{FCTopology[...], ...}]`.*)


(* ::Text:: *)
(*Notice that in this case the value of the option `FinalSubstitutions` is ignored, as replacement rules will be extracted directly from the definition of the topology.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopPakOrder](FCLoopPakOrder.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md)*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*When given a list of `FeynAmpDenominator`-integrals, the function will merely group identical integrals into sublists*)


ints={FAD[{p1,m1}],FAD[{p1+q,m1}],FAD[{p1,m2}]}


FCLoopFindIntegralMappings[ints,{p1}]


(* ::Text:: *)
(*The following 3 integrals look rather different from each other, but are actually identical*)


ints={FAD[p1] FAD[p1-p3-p4] FAD[p4] FAD[p3+q1]FAD[{p3,m1}] FAD[{p1-p4,m1}] FAD[{p1+q1,0},{p1+q1,0}],
FAD[p4]FAD[p1-p3+q1] FAD[p3+q1] FAD[p1+p4+q1] FAD[{p3,m1}] FAD[{p1+q1,m1}] FAD[{p1+p4+2 q1,0},{p1+p4+2 q1,0}],
FAD[p1] FAD[p4-2 q1] FAD[p3+q1] FAD[p1-p3-p4+2 q1] FAD[{p3,m1}] FAD[{p1-p4+2 q1,m1}] FAD[{p1+q1,0},{p1+q1,0}]}


FCLoopFindIntegralMappings[ints,{p1,p3,p4}]


(* ::Text:: *)
(*If the input is a list of `GLI`-integrals, `FCLoopFindIntegralMappings` will return a list containing two sublists. The former will be a list of replacement rules while the latter will contain all unique master integrals*)


ClearAll[topo1,topo2];
topos={
FCTopology[topo1,{SFAD[{p1,m^2}],SFAD[{p2,m^2}]},{p1,p2},{},{},{}],
FCTopology[topo2,{SFAD[{p3,m^2}],SFAD[{p4,m^2}]},{p3,p4},{},{},{}]
}


glis={GLI[topo1,{1,1}],GLI[topo1,{1,2}],GLI[topo1,{2,1}],GLI[topo2,{1,1}],GLI[topo2,{2,2}]}


FCLoopFindIntegralMappings[glis,topos]


(* ::Text:: *)
(*This behavior can be turned off by setting the value of the option `List` to `True`*)


FCLoopFindIntegralMappings[glis,topos,List->True]


(* ::Text:: *)
(*In practice, one usually has a list of preferred integrals onto which one would like to map the occurring master integrals. Such integrals can be specified via the `PreferredIntegrals` options*)


FCLoopFindIntegralMappings[glis,topos,PreferredIntegrals->{GLI[topo2,{1,1}],GLI[topo2,{2,1}]}]


(* ::Text:: *)
(*The indices of `GLI`s do not have to be integers*)


topos={
FCTopology[prop2LtopoG20, {SFAD[{{p1, 0}, {0, 1}, 1}], 
SFAD[{{p1 + q1, 0}, {m3^2, 1}, 1}], SFAD[{{p3, 0}, {0, 1}, 1}], 
SFAD[{{p3 + q1, 0}, {0, 1}, 1}], SFAD[{{p1 - p3, 0}, {0, 1}, 1}]}, 
{p1, p3}, {q1}, {}, {}], 
FCTopology[prop2LtopoG21, {SFAD[{{p1, 0}, {m1^2, 1}, 1}], 
SFAD[{{p1 + q1, 0}, {m3^2, 1}, 1}], 
SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p3 + q1, 0}, {0, 1}, 1}], 
SFAD[{{p1 - p3, 0}, {0, 1}, 1}]}, {p1, p3}, {q1}, {}, {}]
}


FCLoopFindIntegralMappings[{GLI[prop2LtopoG21,{0,n1,n2,n3,n4}],
GLI[prop2LtopoG20,{0,n1,n2,n3,n4}]},topos]


(* ::Text:: *)
(*It is also possible to find mappings for factorizing integrals, provided that suitable products of integrals are given as preferred integrals*)


topos={FCTopology[prop2Ltopo31313,{SFAD[{{
I p1,0},{-m3^2,-1},1}],SFAD[{{I (p1+q1),0},{-m1^2,-1},1}],SFAD[{{
I p3,0},{-m3^2,-1},1}],SFAD[{{I 
(p3+q1),0},{-m1^2,-1},1}],SFAD[{{
I (p1-p3),0},{-m3^2,-1},1}]},{p1,p3},{q1},{SPD[q1,q1]->m1^2},{}],
FCTopology[tad1Ltopo2,{SFAD[{{I p1,0},{-m3^2,-1},1}]},{p1},{},{SPD[q1,q1]->m1^2},{}]}


(* ::Text:: *)
(*Here we ask the function to map all products of two 1-loop tadpoles to `GLI[tad1Ltopo2,{1}]^2`*)


FCLoopFindIntegralMappings[{GLI[tad1Ltopo2,{1}]^2,
GLI[prop2Ltopo31313,{0,0,1,0,1}]},topos,PreferredIntegrals->{GLI[tad1Ltopo2,{1}]^2}]
