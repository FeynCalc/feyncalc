 
(* ::Section:: *)
(* PIDelta *)
(* ::Text:: *)
(*PIDelta[] PIDelta.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*PauliChain, PCHN, PauliIndex, PauliIndexDelta, PauliChainJoin, PauliChainExpand, PauliChainFactor.*)



(* ::Subsection:: *)
(* Examples *)



PIDelta[i,j]

PIDelta[i,i]

PauliChainJoin[%]

PIDelta[i,j]^2

PauliChainJoin[%]

PIDelta[i,j]PIDelta[j,k]

PauliChainJoin[%]

PIDelta[i2,i3]PIDelta[i4,i5]  PCHN[i7,PauliXi[I]] PauliChain[PauliEta[-I],PauliIndex[i0]] PauliChain[PauliSigma[CartesianIndex[a]],PauliIndex[i1],PauliIndex[i2]] PauliChain[PauliSigma[CartesianIndex[b]],PauliIndex[i5],PauliIndex[i6]] PauliChain[m+PauliSigma[CartesianMomentum[p]],PauliIndex[i3],PauliIndex[i4]]

PauliChainJoin[%]

PauliChainJoin[% PIDelta[i0,i1]]

PauliChainJoin[% PIDelta[i7,i6]]
