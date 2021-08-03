(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindPakMappings*)


(* ::Text:: *)
(*`FCLoopFindPakMappings[{int1, int2, ...}, {p1, p2, ...}]` finds mappings between scalar multiloop-integrals `int1, int2, ...` that depend on the loop momenta `p1, p2, ...` using the algorithm of Alexey Pak (arXiv:1111.0868).*)


(* ::Text:: *)
(*The current implementation is based on the `FindEquivalents` function from FIRE 6 (arXiv:1901.07808)*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder).*)


(* ::Subsection:: *)
(*Examples*)


ints={FAD[{p1,m1}],FAD[{p1+q,m1}],FAD[{p1,m2}]}


FCLoopFindPakMappings[ints,{p1}]


ints={FAD[p1] FAD[p1-p3-p4] FAD[p4] FAD[p3+q1]FAD[{p3,m1}] FAD[{p1-p4,m1}] FAD[{p1+q1,0},{p1+q1,0}],
FAD[p4]FAD[p1-p3+q1] FAD[p3+q1] FAD[p1+p4+q1] FAD[{p3,m1}] FAD[{p1+q1,m1}] FAD[{p1+p4+2 q1,0},{p1+p4+2 q1,0}],
FAD[p1] FAD[p4-2 q1] FAD[p3+q1] FAD[p1-p3-p4+2 q1] FAD[{p3,m1}] FAD[{p1-p4+2 q1,m1}] FAD[{p1+q1,0},{p1+q1,0}]}


FCLoopFindPakMappings[ints,{p1,p3,p4},FCE->True]


FAD[q,q-p]
FCLoopIBPReducableQ[FCI[%]]


FAD[{q,0,2},q-p]
FCLoopIBPReducableQ[FCI[%]]
