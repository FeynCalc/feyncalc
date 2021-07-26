 
(* ::Section:: *)
(*DiracChainFactor*)
(* ::Text:: *)
(*`DiracChainFactor[exp]` factors out all expressions inside a `DiracChain` to which the chain doesn't apply. For example, all objects that are not Dirac matrices can be safely factrored out from every Dirac chain.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta), [DiracChainJoin](DiracChainJoin), [DiracChainCombine](DiracChainCombine), [DiracChainExpand](DiracChainExpand).*)



(* ::Subsection:: *)
(*Examples*)


DCHN[FV[p,\[Nu]]GA[\[Mu]].GA[\[Nu]].GA[\[Mu]],i,j] 
DiracChainFactor[%]
