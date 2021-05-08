 
(* ::Section:: *)
(* FCLoopSplit *)
(* ::Text:: *)
(*FCLoopSplit[exp, {q1, q2, ...}]  separates exp into following four pieces: 1) 	terms that are free of loop integrals2) 	terms with scalar loop integrals3) 	terms with tensor loop integrals, where all loop momenta are contracted 4) 	terms with tensor loop integrals, where at least some loop momenta have free indicesThe result is returned as a list with the 4 above elements..*)


(* ::Subsection:: *)
(* Examples *)
FVD[q,\[Mu]] FAD[{q,m}]

FCLoopSplit[%,{q}]

x+GSD[p+q] FAD[{q,m}]

FCLoopSplit[%,{q}]
