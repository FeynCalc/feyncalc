 
(* ::Section:: *)
(* FCLoopBasisIncompleteQ *)
(* ::Text:: *)
(*FCLoopBasisIncompleteQ[int, {q1, q2, ...}] checks if the propagators of the loop integral int( that depends on the loop momenta q1,q2,...) do not form a basis..*)


(* ::Subsection:: *)
(* Examples *)


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
