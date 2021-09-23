(* ::Package:: *)

 


(* ::Section:: *)
(*FCFADiracChainJoin*)


(* ::Text:: *)
(*`FCFADiracChainJoin[exp]` processes the output of FeynArts (after `FCFAConvert`) with explicit Dirac indices and joins matrices and spinors into closed chains. This is necessary e. g. for models with 4-fermion operators, where FeynArts cannot determine the correct relative signs. When two matrices have a common index but the positions do not match, as in $A_{ij} B_{ik}$, it is assumed that we can take the charge conjugate transposed of either matrix to obtain, e.g. $\left(C A^T C^{-1}\right)_{ji} B_{ik}$ or $\left(C B^TC^{-1}\right)_{ki} A_{ij}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DIDelta](DIDelta.md), [DiracChainCombine](DiracChainCombine.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md), [DiracChainJoin](DiracChainJoin.md), [FCCCT](FCCCT.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Create a closed chain for the 1-loop electron self-energy*)


-(1/(16 \[Pi]^4)) I el^2 DCHN[Spinor[-Momentum[p,D],me,1],Dir1] DCHN[Spinor[Momentum[q,D],me,1],Dir2] DCHN[GAD[Lor1],Dir1,Dir3] DCHN[GAD[Lor2],Dir2,Dir4] DCHN[me-GSD[k],Dir3,Dir4] FAD[{k,me},k-q] MTD[Lor1,Lor2]
res=FCFADiracChainJoin[%]


(* ::Text:: *)
(*Sometimes the ordering of the spinors is not the one wants to have. However, we can always transpose the chains to reorder the spinors as we like, which doesn't change the final result*)


SpinorChainTranspose[res,Select->{{Spinor[__],Spinor[__]}}]


(* ::Text:: *)
(*Using patterns in the `Select` option one can create very fine-grained criteria for transposing the chains.*)
