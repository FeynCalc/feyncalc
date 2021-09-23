(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisIncompleteQ*)


(* ::Text:: *)
(*`FCLoopBasisIncompleteQ[int, {q1, q2, ...}]` checks whether the loop integral or topology `int` lacks propagators need to have a linearly independent basis .*)


(* ::Text:: *)
(*The input can also consist of an `FCTopology` object or a list thereof.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopBasisOverdeterminedQ](FCLoopBasisOverdeterminedQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FAD[{q1,m1}]
FCLoopBasisIncompleteQ[%,{q1}]


SPD[q1,l]FAD[{q1,m1},{q1-l+p,m}]
FCLoopBasisIncompleteQ[%,{q1}]


FAD[{q1,m1},{q2,m2}]
FCLoopBasisIncompleteQ[%,{q1,q2}]


FAD[q1,q2,{q1-l1,m1},{q2-l2,m2}]
FCLoopBasisIncompleteQ[%,{q1,q2}]


CSPD[q1,l]CFAD[{q1,m1},{q1-l+p,m}]
FCLoopBasisIncompleteQ[%,{q1}]


SFAD[{q1,m1},{q2,m2}]
FCLoopBasisIncompleteQ[%,{q1,q2}]


FCLoopBasisIncompleteQ[FCTopology[topo,{FAD[p1],
FAD[p2],FAD[p1-q],FAD[p2-q]},{p1,p2},{q},{},{}]]


FCLoopBasisIncompleteQ[{
FCTopology[topo1,{FAD[p1],FAD[p2],FAD[p1-q],FAD[p2-q]},{p1,p2},{q},{},{}],
FCTopology[topo2,{FAD[p1],FAD[p2],FAD[p1-q],FAD[p2-p1]},{p1,p2},{q},{},{}],
FCTopology[topo3,{FAD[p1],FAD[p1-q]},{p1},{q},{},{}]
}]
