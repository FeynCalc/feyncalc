 
(* ::Section:: *)
(*DiracChainFactor*)
(* ::Text:: *)
(*`DiracChainFactor[exp]` factors out all expressions inside a `DiracChain` to which the chain doesn't apply. For example, all objects that are not Dirac matrices can be safely factrored out from every Dirac chain.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md).*)



(* ::Subsection:: *)
(*Examples*)


DCHN[FV[p,\[Nu]]GA[\[Mu]].GA[\[Nu]].GA[\[Mu]],i,j] 
DiracChainFactor[%]
