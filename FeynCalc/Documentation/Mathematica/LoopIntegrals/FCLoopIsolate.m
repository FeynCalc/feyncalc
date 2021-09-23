 
(* ::Section:: *)
(*FCLoopIsolate*)
(* ::Text:: *)
(*`FCLoopIsolate[expr, {q1, q2, ...}]` wraps loop integrals into heads specified by the user. This is useful when you want to know which loop integrals appear in the given expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopExtract](FCLoopExtract.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[GSD[q-p1].(GSD[q-p2]+M).GSD[p3]SPD[q,p2]FAD[q,q-p1,{q-p2,m}]]
FCLoopIsolate[%,{q},Head->loopInt]
Cases2[%,loopInt]


TID[FVD[q,\[Mu]]FVD[q,\[Nu]]FAD[{q,m},{q+p,m},{q+r,m}],q,UsePaVeBasis->True]
FCLoopIsolate[%,{q},Head->l]
Cases2[%,l]


SPD[q,q]^2FAD[{q,m}]+SPD[q,q]
FCLoopIsolate[%,{q},DropScaleless->True]


a FAD[{q1,m},{q2,m}]+b FAD[{q1,m,2}]
FCLoopIsolate[%,{q1,q2}]
FCLoopIsolate[%%,{q1,q2},MultiLoop->True]
