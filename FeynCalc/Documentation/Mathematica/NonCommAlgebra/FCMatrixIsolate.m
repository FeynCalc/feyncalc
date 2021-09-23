(* ::Package:: *)

 


(* ::Section:: *)
(*FCMatrixIsolate*)


(* ::Text:: *)
(*`FCMatrixIsolate[exp]` wraps the occurring Dirac, Pauli and color objects into heads specified by the user.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCDiracIsolate](FCDiracIsolate.md), [FCColorIsolate](FCColorIsolate.md), [FCPauliIsolate](FCPauliIsolate.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=-e eQ gs Spinor[Momentum[k2],mu,1] . GS[Polarization[k1,-I,
Transversality->True]] . (mu+GS[k1+k2]) . GS[Polarization[p2,
I]] . Spinor[Momentum[p1],mu,1]FAD[{-k1-k2,mu},Dimension->4]*
SUNTF[{Glu3},Col4,Col1]-e eQ gs DCHN[Spinor[Momentum[k2],mu,
1],i] DCHN[GS[Polarization[p2,I]] . (mu+GS[k2-p2]) . GS[Polarization[k1,
-I,Transversality->True]],i,j] DCHN[Spinor[Momentum[p1],mu,1],j]*
 FAD[{-k2+p2,mu},Dimension->4] SUNTF[{Glu3},Col4,Col1]


FCMatrixIsolate[ex,FCDiracIsolate->{dch},FCColorIsolate->{cch},
FCPauliIsolate->{pch},Head->re,FCE->True]
