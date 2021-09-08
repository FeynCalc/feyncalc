(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopSolutionList*)


(* ::Text:: *)
(*`FCLoopSolutionList[loopList, reversedRepIndexList, canIndexList, uniqueCanIndexList}, solsList]` is an auxiliary internal function that uses the output of FCLoopCanonicalize and the list of simplified integrals solsList to create the substitution list of type `"Integral" -> "simplified Integral"`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopCanonicalize](FCLoopCanonicalize.md).*)


(* ::Subsection:: *)
(*Examples*)


li=FCLoopCanonicalize[myHead[FVD[q,\[Mu]] FVD[q,\[Nu]] FAD[q,{q+p,m}]]+myHead[FVD[q,\[Rho]] FVD[q,\[Sigma]] FAD[q,{q+p,m}]],q,myHead] 


FCLoopSolutionList[li,prefactor (li[[4]]/.myHead->Identity/.q->p),Dispatch->False]
