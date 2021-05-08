 
(* ::Section:: *)
(* DiracChainCombine *)
(* ::Text:: *)
(*`DiracChainCombine[exp]` is (nearly) the inverse operation to `DiracChainExpand`.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracChain, DCHN, DiracIndex, DiracIndexDelta, DIDelta, DiracChainJoin, DiracChainExpand, DiracChainFactor.*)



(* ::Subsection:: *)
(* Examples *)


(DCHN[GSD[q],Dir3,Dir4] FAD[{k,me}])/(2 SPD[q,q])+1/(2 SPD[q,q]) FAD[k,{k-q,me}] (-2 DCHN[GSD[q],Dir3,Dir4] SPD[q,q]+2 DCHN[1,Dir3,Dir4] me SPD[q,q]+DCHN[GSD[q],Dir3,Dir4] (-me^2+SPD[q,q]))
DiracChainCombine[%]
