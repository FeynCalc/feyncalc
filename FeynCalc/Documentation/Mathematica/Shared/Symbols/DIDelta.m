(* ::Package:: *)

 


(* ::Section:: *)
(*DIDelta*)


(* ::Text:: *)
(*`DIDelta[i, j]` is the Kronecker-delta in the Dirac space.*)


(* ::Text:: *)
(*`DIDelta[i,j]` is transformed into `DiracDelta[DiracIndex[i],DiracIndex[j]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracChain](DiracChain.md), [DCHN](DCHN.md), [DiracIndex](DiracIndex.md), [DiracIndexDelta](DiracIndexDelta.md), [DiracChainJoin](DiracChainJoin.md), [DiracChainExpand](DiracChainExpand.md), [DiracChainFactor](DiracChainFactor.md).*)


(* ::Subsection:: *)
(*Examples*)


DIDelta[i,j]


DIDelta[i,i]
DiracChainJoin[%]


DIDelta[i,j]^2
DiracChainJoin[%]


DIDelta[i,j]DIDelta[j,k]
DiracChainJoin[%]


ex=DCHN[SpinorUBar[p,m],i0]DCHN[GA[\[Mu]],i1,i2]DCHN[GS[p]+m,i3,i4]DCHN[GA[\[Nu]],i5,i6]DIDelta[i2,i3]DIDelta[i4,i5]DCHN[i7,SpinorV[q]]


ex//FCI//StandardForm


DiracChainJoin[ex]


DiracChainJoin[ex DIDelta[i0,i1]]


DiracChainJoin[ex DIDelta[i7,i6]]
