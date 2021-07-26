 
(* ::Section:: *)
(*DiracChainJoin*)
(* ::Text:: *)
(*`DiracChainJoin[exp]` joins chains of Dirac matrices with explicit Dirac indices wrapped with a head `DiracChain`. Notice that `DiracChainJoin` is not suitable for creating closed Dirac chains out of the FeynArts output with explicit Dirac indices, e.g. when the model contains 4-fermion operators. Use `FCFADiracChainJoin` for that.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracChain](DiracChain), [DCHN](DCHN), [DiracIndex](DiracIndex), [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta), [DiracChainCombine](DiracChainCombine), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor), [FCFADiracChainJoin](FCFADiracChainJoin).*)



(* ::Subsection:: *)
(*Examples*)



DCHN[SpinorUBar[p1,m1],i] DCHN[GAD[\[Mu]].GAD[\[Nu]],i,j] DCHN[j,SpinorV[p2,m2]]
DiracChainJoin[%]
