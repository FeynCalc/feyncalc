(* ::Package:: *)

(* ::Section:: *)
(*FCLoopSelectTopology*)


(* ::Text:: *)
(*`FCLoopSelectTopology[int, topos]` selects the topology that matches the `GLI` `int` from a list of topologies `topos`.*)


(* ::Text:: *)
(*The first argument can be also a list, in which case the function will return a list of matching topologies.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


topos={FCTopology[topo2,{FAD[{p1,m}],FAD[{p1-q,m}]},{p1},{q},{},{}],
FCTopology[topo1,{FAD[{p1,m}],FAD[{p1-q,m}]},{p1},{q},{SPD[q]->M^2,SPD[q2]->M2^2},{}]}


FCLoopSelectTopology[{GLI[topo2,{2,2}]},topos]


topos={FCTopology[prop2Ltopo13311,{SFAD[{{I*p1,0},{-m1^2,-1},1}],
SFAD[{{I*(p1+q1),0},{-m3^2,-1},1}],SFAD[{{I*p3,0},{-m3^2,-1},1}],
SFAD[{{I*(p3+q1),0},{-m1^2,-1},1}],SFAD[{{I*(p1-p3),0},
{-m1^2,-1},1}]},{p1,p3},{q1},{SPD[q1,q1]->m1^2},{}]}


(* ::Text:: *)
(*Products of `GLI`s in the first argument are also supported.*)


FCLoopSelectTopology[{GLI[prop2Ltopo13311,{1,0,0,0,0}]^2,GLI[prop2Ltopo13311,{1,1,1,1,1}]},topos,FCE->True]
