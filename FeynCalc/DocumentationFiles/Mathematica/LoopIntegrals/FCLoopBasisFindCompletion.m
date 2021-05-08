 
(* ::Section:: *)
(* FCLoopBasisFindCompletion *)
(* ::Text:: *)
(*FCLoopBasisFindCompletion[int, {q1, q2, ...}] determines propagators that need to be included in the loop integral int (that depends on the loop momenta q1,q2,...), to ensure that the propagators of int form a basis. For integrals with propagators that do not form a basis, such a completion must be found prior to processing those integrals with tools that do Integration-By-Parts (IBP) reduction (e.g. FIRE). Furthermore, int must not contain propagators that are linearly dependent..*)


(* ::Subsection:: *)
(* Examples *)
FAD[q,{q-p+l,m}]

FCLoopBasisFindCompletion[%,{q}]

FAD[{q1,m1},{q2,m2}]

FCLoopBasisFindCompletion[%,{q1,q2}]

FAD[q1+p,q2-k] SPD[q1,q2]

FCLoopBasisFindCompletion[%,{q1,q2},Method->{FAD[{q2+k,m}],FAD[{q1-p,m}],SPD[p,q2],SPD[k,q1]}]


(* ::Text:: *)
(*Cartesian integrals are also supported.*)


CFAD[q1,q2,{q1-l1,m1},{q2-l2,m2}]

FCLoopBasisFindCompletion[%,{q1,q2}]
