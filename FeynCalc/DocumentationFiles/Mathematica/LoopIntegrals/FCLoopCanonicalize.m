(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopCanonicalize*)


(* ::Text:: *)
(*`FCLoopCanonicalize[exp, q, loopHead]` is an auxiliary internal function that canonicalizes indices of 1-loop integrals with loop momentum `q` that are wrapped with `loopHead`. The output is given as a list of 4 entries, of which the last one contains a list of all the unique 1-loop integrals in the given expression. After those are simplified, the original output of `FCLoopCanonicalize` together with the list of the simplified unique integrals should be inserted into `FCLoopSolutionList` to obtain the final replacement list that will be applied to the original expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopSolutionList](FCLoopSolutionList.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopCanonicalize[myHead[FVD[q,\[Mu]]],q,myHead]


FCLoopCanonicalize[myHead[FVD[q,\[Mu]] FVD[q,\[Nu]] FAD[q,{q+p,m}]]+myHead[FVD[q,\[Rho]] FVD[q,\[Sigma]] FAD[q,{q+p,m}]],q,myHead]
