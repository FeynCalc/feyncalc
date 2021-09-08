(* ::Package:: *)

 


(* ::Section:: *)
(*DiracIndexDelta*)


(* ::Text:: *)
(*`DiracIndexDelta[DiracIndex[i], DiracIndex[j]]` is the Kronecker-delta in the Dirac space with two explicit Dirac indices `i` and `j`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DIDelta](DIDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracIndexDelta[DiracIndex[i],DiracIndex[j]]


ex=DiracIndexDelta[DiracIndex[i],DiracIndex[j]]^2


DiracChainJoin[ex]


DiracChainJoin[ex,TraceOfOne->D]


ex=DiracIndexDelta[DiracIndex[i],DiracIndex[j]]DiracIndexDelta[DiracIndex[j],DiracIndex[k]]


DiracChainJoin[ex]


ex=DiracIndexDelta[DiracIndex[i2],DiracIndex[i3]] DiracIndexDelta[DiracIndex[i4],DiracIndex[i5]] DiracChain[DiracIndex[i7],Spinor[-Momentum[q],0,1]] DiracChain[Spinor[Momentum[p],m,1],DiracIndex[i0]] DiracChain[DiracGamma[LorentzIndex[\[Mu]]],DiracIndex[i1],DiracIndex[i2]] DiracChain[DiracGamma[LorentzIndex[\[Nu]]],DiracIndex[i5],DiracIndex[i6]] DiracChain[m+DiracGamma[Momentum[p]],DiracIndex[i3],DiracIndex[i4]]


DiracChainJoin[ex]


DiracChainJoin[ex DIDelta[i0,i1]]


DiracChainJoin[ex DIDelta[i7,i6]]
