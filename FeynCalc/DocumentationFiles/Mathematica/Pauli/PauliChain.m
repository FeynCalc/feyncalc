 
(* ::Section:: *)
(* PauliChain *)
(* ::Text:: *)
(*PauliChain[x, i, j] denotes a chain of Pauli matrices x, where the Pauli indices i and j are explicit..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*PCHN, PauliIndex, PauliIndexDelta, PauliChainJoin, PauliChainExpand, PauliChainFactor.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*A standalone Pauli matrix*)


PauliChain[PauliSigma[CartesianIndex[a]],PauliIndex[i],PauliIndex[j]]


(* ::Text:: *)
(*A chain of Pauli matrices with open indices*)


PauliChain[PauliSigma[CartesianIndex[a,D-1],D-1].PauliSigma[CartesianIndex[b,D-1],D-1],PauliIndex[i],PauliIndex[j]]


(* ::Text:: *)
(*A PauliChain with only two arguments denotes a spinor component*)


PauliChain[PauliXi[-I],PauliIndex[i]]

PauliChain[PauliEta[-I],PauliIndex[i]]

PauliChain[PauliIndex[i],PauliXi[I]]

PauliChain[PauliIndex[i],PauliEta[I]]


(* ::Text:: *)
(*The chain may also be partially open or closed*)


PauliChain[PauliSigma[CartesianIndex[a]].(m+PauliSigma[CartesianMomentum[p]]).PauliSigma[CartesianIndex[b]],PauliXi[-I],PauliIndex[j]]

PauliChain[PauliSigma[CartesianIndex[a]].(m+PauliSigma[CartesianMomentum[p]]).PauliSigma[CartesianIndex[b]],PauliIndex[i],PauliXi[I]]

PauliChain[PauliSigma[CartesianIndex[a]].(m+PauliSigma[CartesianMomentum[p]]).PauliSigma[CartesianIndex[b]],PauliXi[-I],PauliEta[I]]

PauliChain[1,PauliXi[-I],PauliEta[I]]
