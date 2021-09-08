(* ::Package:: *)

 


(* ::Section:: *)
(*PaVeUVPart*)


(* ::Text:: *)
(*`PaVeUVPart[expr]` replaces all occurring Passarino-Veltman functions by their explicit values, where only the UV divergent part is preserved, while possible IR divergences and the finite part are discarded. The function uses the algorithm from [arXiv:hep-ph/0609282](https://arxiv.org/abs/hep-ph/0609282) by G. Sulyok. This allows to treat Passarino-Veltman of arbitrary rank and multiplicity*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PaVe](PaVe.md), [PaVeReduce](PaVeReduce.md).*)


(* ::Subsection:: *)
(*Examples*)


PaVeUVPart[A0[m^2]]


PaVeUVPart[x+y B0[SPD[p,p],0,M^2]]


PaVe[0,0,{p10,p12,p20},{m1^2,m2^2,m3^2}]
PaVeUVPart[%]


PaVe[0,0,0,0,0,0,{p10,p12,p23,0,p20,p13},{m1^2,m2^2,m3^2,m4^2}]
PaVeUVPart[%]


int=FVD[k+p,rho]FVD[k+p,si]FAD[k,{k+p,0,2}]
TID[int,k,UsePaVeBasis->True]
%//PaVeUVPart[#,FCE->True]&
