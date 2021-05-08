 
(* ::Section:: *)
(* DiracIndexDelta *)
(* ::Text:: *)
(*DiracIndexDelta[DiracIndex[i], DiracIndex[j]] is the Kronecker-delta in the Dirac space with two explicit Dirac indices i and j..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracChain, DCHN, DiracIndex, DIDelta, DiracChainJoin, DiracChainCombine, DiracChainExpand, DiracChainFactor.*)



(* ::Subsection:: *)
(* Examples *)



DiracIndexDelta[DiracIndex[i],DiracIndex[j]]

DiracIndexDelta[DiracIndex[i],DiracIndex[j]]^2

DiracChainJoin[%]

DiracChainJoin[%%,TraceOfOne->D]

DiracIndexDelta[DiracIndex[i],DiracIndex[j]]DiracIndexDelta[DiracIndex[j],DiracIndex[k]]

DiracChainJoin[%]

DiracIndexDelta[DiracIndex[i2],DiracIndex[i3]] DiracIndexDelta[DiracIndex[i4],DiracIndex[i5]] DiracChain[DiracIndex[i7],Spinor[-Momentum[q],0,1]] DiracChain[Spinor[Momentum[p],m,1],DiracIndex[i0]] DiracChain[DiracGamma[LorentzIndex[\[Mu]]],DiracIndex[i1],DiracIndex[i2]] DiracChain[DiracGamma[LorentzIndex[\[Nu]]],DiracIndex[i5],DiracIndex[i6]] DiracChain[m+DiracGamma[Momentum[p]],DiracIndex[i3],DiracIndex[i4]]

DiracChainJoin[%]

DiracChainJoin[% DIDelta[i0,i1]]

DiracChainJoin[% DIDelta[i7,i6]]
