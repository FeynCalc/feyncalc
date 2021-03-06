 
(* ::Section:: *)
(* DiracChainExpand *)
(* ::Text:: *)
(*`DiracChainExpand[exp]` expands all Dirac chains with explicit indices using linearity, e.g. `DCHN[GA[p1]+GA[p2]+m,i,j]` becomes `DCHN[GA[p1],i,j]+DCHN[GA[p2],i,j]+m*DCHN[1,i,j]`.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracChain, DCHN, DiracIndex, DiracIndexDelta, DIDelta, DiracChainJoin, DiracChainCombine, DiracChainFactor.*)



(* ::Subsection:: *)
(* Examples *)


DCHN[(GS[p]+m).GA[mu],i,j]
DiracChainExpand[%]
