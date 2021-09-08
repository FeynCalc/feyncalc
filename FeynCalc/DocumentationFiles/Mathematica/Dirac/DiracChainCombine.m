 
(* ::Section:: *)
(*DiracChainCombine*)
(* ::Text:: *)
(*`DiracChainCombine[exp]` is (nearly) the inverse operation to `DiracChainExpand`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).*)



(* ::Subsection:: *)
(*Examples*)


(DCHN[GSD[q],Dir3,Dir4] FAD[{k,me}])/(2 SPD[q,q])+1/(2 SPD[q,q]) FAD[k,{k-q,me}] (-2 DCHN[GSD[q],Dir3,Dir4] SPD[q,q]+2 DCHN[1,Dir3,Dir4] me SPD[q,q]+DCHN[GSD[q],Dir3,Dir4] (-me^2+SPD[q,q]))
DiracChainCombine[%]
