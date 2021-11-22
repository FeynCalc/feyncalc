(* ::Package:: *)

 


(* ::Section:: *)
(*FCCanonicalizeDummyIndices*)


(* ::Text:: *)
(*`FCCanonicalizeDummyIndices[expr]` canonicalizes dummy indices in the expression.*)


(* ::Text:: *)
(*Following index types are supported: `LorentzIndex`, `CartesianIndex`, `SUNIndex`, `SUNFIndex`, `DiracIndex`, `PauliIndex`*)


(* ::Text:: *)
(*In the case of Lorentz indices the option `Momentum` provides a possibility to limit the canonicalization only to particular `Momenta`. The option `LorentzIndexNames` can be used to assign specific names to  the canonicalized indices, to have say $\mu$, $\nu$, $\rho$ etc. instead of some random names.*)


(* ::Text:: *)
(*For other index types the corresponding options are called `CartesianIndexNames`, `SUNIndexNames`, `SUNFIndexNames`, `DiracIndexNames` and `PauliIndexNames`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Canonicalization of Lorentz indices*)


FVD[q,mu]FVD[p,mu]+FVD[q,nu]FVD[p,nu]+FVD[q,si]FVD[r,si]
FCCanonicalizeDummyIndices[%]//Factor2


Uncontract[SPD[q,p]^2,q,p,Pair->All]
FCCanonicalizeDummyIndices[%,LorentzIndexNames->{\[Mu],\[Nu]}]


(* ::Text:: *)
(*Canonicalization of Cartesian indices*)


CVD[p,i]CVD[q,i]+CVD[p,j]CVD[r,j]
FCCanonicalizeDummyIndices[%]//Factor2


CVD[p,i]CVD[q,i]+CVD[p,j]CVD[r,j]
FCCanonicalizeDummyIndices[%,CartesianIndexNames->{a}]//Factor2


(* ::Text:: *)
(*Canonicalization of color indices*)


SUNT[a,b,a]+SUNT[c,b,c]
FCCanonicalizeDummyIndices[%]


SUNT[a,b,a]+SUNT[c,b,c]
FCCanonicalizeDummyIndices[%,SUNIndexNames->{u}]


(* ::Text:: *)
(*Canonicalization of Dirac indices*)


DCHN[GA[mu],i,j]DCHN[GA[nu],j,k]
FCCanonicalizeDummyIndices[%]


DCHN[GA[mu],i,j]DCHN[GA[nu],j,k]
FCCanonicalizeDummyIndices[%,DiracIndexNames->{a}]


(* ::Text:: *)
(*Canonicalization of Pauli indices*)


PCHN[CSI[a],i,j]PCHN[CSI[b],j,k]
FCCanonicalizeDummyIndices[%]


PCHN[CSI[a],i,j]PCHN[CSI[b],j,k]
FCCanonicalizeDummyIndices[%,PauliIndexNames->{l}]


(* ::Text:: *)
(*Using the option `Head` one can specify which index heads should be canonicalized,*)
(*while the rest will be ignored.*)


(QuantumField[SuperPlus[\[Phi]], PauliIndex[k1], PauliIndex[k2], R,r] . QuantumField[FCPartialD[{CartesianIndex[i], r}],
FCPartialD[{CartesianIndex[i], r}], \[Phi], PauliIndex[k2],PauliIndex[k1], R, r])
FCCanonicalizeDummyIndices[%, CartesianIndexNames -> {j},Head->{CartesianIndex}]



