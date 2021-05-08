 
(* ::Section:: *)
(* DIDelta *)
(* ::Text:: *)
(*DIDelta[i, j] is the Kronecker-delta in the Dirac space. DIDelta[i,j] is transformed into DiracDelta[DiracIndex[i],DiracIndex[j]] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracChain, DCHN, DiracIndex, DiracIndexDelta, DiracChainJoin, DiracChainExpand, DiracChainFactor.*)



(* ::Subsection:: *)
(* Examples *)



DIDelta[i,j]

DIDelta[i,i]

DiracChainJoin[%]

DIDelta[i,j]^2

DiracChainJoin[%]

DIDelta[i,j]DIDelta[j,k]

DiracChainJoin[%]

DCHN[SpinorUBar[p,m],i0]DCHN[GA[\[Mu]],i1,i2]DCHN[GS[p]+m,i3,i4]DCHN[GA[\[Nu]],i5,i6]DIDelta[i2,i3]DIDelta[i4,i5]DCHN[i7,SpinorV[q]]

DCHN[SpinorUBar[p,m],i0]DCHN[GA[\[Mu]],i1,i2]DCHN[GS[p]+m,i3,i4]DCHN[GA[\[Nu]],i5,i6]DIDelta[i2,i3]DIDelta[i4,i5]DCHN[i7,SpinorV[q]]//FCI//StandardForm

DiracChainJoin[%]

DiracChainJoin[% DIDelta[i0,i1]]

DiracChainJoin[% DIDelta[i7,i6]]
