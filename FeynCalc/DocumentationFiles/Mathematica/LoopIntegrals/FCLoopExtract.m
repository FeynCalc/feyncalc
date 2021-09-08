 
(* ::Section:: *)
(*FCLoopExtract*)
(* ::Text:: *)
(*`FCLoopExtract[expr, {q1, q2, ...}, loopHead]` exctracts loop integrals that depend on `q1, q2, ...` from the given expression. The output is given as a list of three entries. The first one contains part of the original expression that consists of irrelevant loop integrals and terms that are free of any loop integrals. The second entry contains relevant loop integrals, where each integral is wrapped into `loopHead`. The third entry is a list of all the unique loop integrals from the second entry and can be used as an input to another function. Note that if loop integrals contain free indices, those will not be canonicalized.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopIsolate.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[GSD[q-p1].(GSD[q-p2]+M).GSD[p3]SPD[q,p2]FAD[q,q-p1,{q-p2,m}]]
FCLoopExtract[%,{q},loopInt]
