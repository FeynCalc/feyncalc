(* ::Package:: *)

 


(* ::Section:: *)
(*FCCanonicalizeDummyIndices*)


(* ::Text:: *)
(*`FCCanonicalizeDummyIndices[expr]`  canonicalizes all dummy Lorentz indices in the expression. The option `Momentum` provides a possibility to limit the canonicalization only to particular `Momenta`.*)


(* ::Text:: *)
(*With the option `LorentzIndexNames` one can provide a list of names to be used for the canonicalized indices, to have say $\mu$, $\nu$, $\rho$ etc. instead of some random names.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md).*)


(* ::Subsection:: *)
(*Examples*)


FVD[q,mu]FVD[p,mu]+FVD[q,nu]FVD[p,nu]+FVD[q,si]FVD[r,si]
FCCanonicalizeDummyIndices[%]//Factor2


Uncontract[SPD[q,p]^2,q,p,Pair->All]
FCCanonicalizeDummyIndices[%,LorentzIndexNames->{\[Mu],\[Nu]}]



