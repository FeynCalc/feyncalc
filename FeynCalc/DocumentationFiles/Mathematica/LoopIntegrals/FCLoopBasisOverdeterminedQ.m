(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopBasisOverdeterminedQ*)


(* ::Text:: *)
(*`FCLoopBasisOverdeterminedQ[int, {q1, q2, ...}]` checks whether the loop integral or topology `int` contains linearly dependent propagators.*)


(* ::Text:: *)
(*The input can also consist of an `FCTopology` object or a list thereof.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopBasisIncompleteQ](FCLoopBasisIncompleteQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FAD[{q1,m1},{q1-l+p,m}]
FCLoopBasisOverdeterminedQ[%,{q1}]


FAD[q1,{q1,m1}]
FCLoopBasisOverdeterminedQ[%,{q1}]


FAD[q1,q2,{q1+l,m1},{q1-l,m1},{q2+l,m1},{q2-l,m1}]
FCLoopBasisOverdeterminedQ[%,{q1,q2}]


FCLoopBasisOverdeterminedQ[FCTopology[topo1,{FAD[p1],FAD[p2],
FAD[p1-q],FAD[p2-q],FAD[p1-p2],FAD[p1+p2+q]},{p1,p2},{q},{},{}]]


FCLoopBasisOverdeterminedQ[{FCTopology[topo1,{FAD[p1],FAD[p2],
FAD[p1-q],FAD[p2-q],FAD[p1-p2],FAD[p1+p2+q]},{p1,p2},{q},{},{}],
FCTopology[topo2,{FAD[p1],FAD[p2],
FAD[p1-q],FAD[p2-q],FAD[p1-p2]},{p1,p2},{q},{},{}]
}]
