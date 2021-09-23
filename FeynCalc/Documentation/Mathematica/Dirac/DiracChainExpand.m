 
(* ::Section:: *)
(*DiracChainExpand*)
(* ::Text:: *)
(*`DiracChainExpand[exp]` expands all Dirac chains with explicit indices using linearity, e.g. `DCHN[GA[p1]+GA[p2]+m,i,j]` becomes `DCHN[GA[p1],i,j]+DCHN[GA[p2],i,j]+m*DCHN[1,i,j]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainFactor](DiracChainFactor.md).*)



(* ::Subsection:: *)
(*Examples*)


DCHN[(GS[p]+m).GA[mu],i,j]
DiracChainExpand[%]
