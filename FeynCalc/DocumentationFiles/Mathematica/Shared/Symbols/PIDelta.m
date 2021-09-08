(* ::Package:: *)

 


(* ::Section:: *)
(*PIDelta*)


(* ::Text:: *)
(*`PIDelta[i,j]` is the Kronecker-delta in the Pauli space. `PIDelta[i,j]` is transformed into `PauliIndexDelta[PauliIndex[i],PauliIndex[j]]` by FeynCalcInternal.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliChain](PauliChain.md), [PCHN](PCHN.md), [PauliIndex](PauliIndex.md), [PauliIndexDelta](PauliIndexDelta.md), [PauliChainJoin](PauliChainJoin.md), [PauliChainExpand](PauliChainExpand.md), [PauliChainFactor](PauliChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


PIDelta[i,j]


PIDelta[i,i]
PauliChainJoin[%]


PIDelta[i,j]^2
PauliChainJoin[%]


PIDelta[i,j]PIDelta[j,k]
PauliChainJoin[%]


ex=PIDelta[i2,i3]PIDelta[i4,i5]  PCHN[i7,PauliXi[I]] PauliChain[PauliEta[-I],PauliIndex[i0]] PauliChain[PauliSigma[CartesianIndex[a]],PauliIndex[i1],PauliIndex[i2]] PauliChain[PauliSigma[CartesianIndex[b]],PauliIndex[i5],PauliIndex[i6]] PauliChain[m+PauliSigma[CartesianMomentum[p]],PauliIndex[i3],PauliIndex[i4]]


PauliChainJoin[ex]


PauliChainJoin[ex PIDelta[i0,i1]]


PauliChainJoin[% PIDelta[i7,i6]]
