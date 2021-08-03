(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopToPakForm*)


(* ::Text:: *)
(*`FCLoopToPakForm[int, {p1, p2, ...}]` determines a canonical $UF$-based representation for the scalar multi-loop integral int using the algorithm of Alexey Pak (arXiv:1111.0868).*)


(* ::Text:: *)
(*The current implementation is based on the FindEquivalents function from FIRE 6 (arXiv:1901.07808)*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakScalelessQ](FCLoopPakScalelessQ).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopToPakForm[FAD[p1,{p3,m1},{p1-p4,m1},p1+q1,p1+q1,p3+q1,p1-p3-p4],
{p1,p3,p4},Names->x,Head->ph,Power->pow,FCE->True]
