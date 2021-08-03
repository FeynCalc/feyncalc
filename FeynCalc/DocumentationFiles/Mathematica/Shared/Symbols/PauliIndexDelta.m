(* ::Package:: *)

 


(* ::Section:: *)
(*PauliIndexDelta*)


(* ::Text:: *)
(*`PauliIndexDelta[PauliIndex[i], PauliIndex[j]]` is the Kronecker-delta in the Pauli space with two explicit Pauli indices `i` and `j`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliChain](PauliChain), [PCHN](PCHN), [PauliIndex](PauliIndex), [DIDelta](DIDelta), [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainExpand](PauliChainExpand), [PauliChainFactor](PauliChainFactor).*)


(* ::Subsection:: *)
(*Examples*)


PauliIndexDelta[PauliIndex[i],PauliIndex[j]]


PauliIndexDelta[PauliIndex[i],PauliIndex[j]]^2
PauliChainJoin[%]
PauliChainJoin[%%,TraceOfOne->D]


PauliIndexDelta[PauliIndex[i],PauliIndex[j]]PauliIndexDelta[PauliIndex[j],PauliIndex[k]]
PauliChainJoin[%]


PauliChain[PauliEta[-I],PauliIndex[i0]]PIDelta[i0,i1]//FCI//PauliChainJoin


PauliIndexDelta[PauliIndex[i2],PauliIndex[i3]] PauliIndexDelta[PauliIndex[i4],PauliIndex[i5]] PauliChain[PauliIndex[i7],PauliXi[I]] PauliChain[PauliEta[-I],PauliIndex[i0]] PauliChain[PauliSigma[CartesianIndex[a]],PauliIndex[i1],PauliIndex[i2]] PauliChain[PauliSigma[CartesianIndex[b]],PauliIndex[i5],PauliIndex[i6]] PauliChain[m+PauliSigma[CartesianMomentum[p]],PauliIndex[i3],PauliIndex[i4]]
PauliChainJoin[%]


PauliChainJoin[% PIDelta[i0,i1]]


PauliChainJoin[% PIDelta[i7,i6]]
