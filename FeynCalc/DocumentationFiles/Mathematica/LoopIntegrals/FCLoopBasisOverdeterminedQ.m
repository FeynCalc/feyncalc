 
(* ::Section:: *)
(* FCLoopBasisOverdeterminedQ *)
(* ::Text:: *)
(*`FCLoopBasisOverdeterminedQ[int, {q1, q2, ...}]` checks if the propagators of the loop integral `int` (that depends on the loop momenta `q1, q2, ...`) are linearly dependent.*)


(* ::Subsection:: *)
(* Examples *)


FAD[{q1,m1},{q1-l+p,m}]
FCLoopBasisOverdeterminedQ[%,{q1}]


FAD[q1,{q1,m1}]
FCLoopBasisOverdeterminedQ[%,{q1}]


FAD[q1,q2,{q1+l,m1},{q1-l,m1},{q2+l,m1},{q2-l,m1}]
FCLoopBasisOverdeterminedQ[%,{q1,q2}]
