(* ::Package:: *)

 


(* ::Section:: *)
(*InverseMellin*)


(* ::Text:: *)
(*`InverseMellin[exp, y]` performs the inverse Mellin transform of polynomials in OPE. The inverse transforms are not calculated but a table-lookup is done.*)


(* ::Text:: *)
(*WARNING: do not "trust" the results for the inverse Mellin transform involving SumT's; there is an unresolved inconsistency here (related to $(-1)^{m}$).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DeltaFunction](DeltaFunction.md), [Integrate2](Integrate2.md), [OPEm](OPEm.md), [SumS](SumS.md), [SumT](SumT.md).*)


(* ::Subsection:: *)
(*Examples*)


InverseMellin[1/OPEm,y]


InverseMellin[1/(OPEm+3),y]


InverseMellin[1,y]


InverseMellin[1/OPEm^4,y]


InverseMellin[1/OPEm+1,y]


InverseMellin[1/i+1,y,i]


(* ::Text:: *)
(*The inverse operation to `InverseMellin` is done by `Integrate2`.*)


Integrate2[InverseMellin[1/OPEm,y],{y,0,1}]


(* ::Text:: *)
(*Below is a list of all built-in basic inverse Mellin transforms .*)


list={1,1/(OPEm+n),1/(-OPEm+n),PolyGamma[0,OPEm],SumS[1,-1+OPEm],
SumS[1,-1+OPEm]/(OPEm-1),SumS[1,-1+OPEm]/(1-OPEm),SumS[1,-1+OPEm]/(OPEm+1),
SumS[1,-1+OPEm]/OPEm^2,SumS[1,-1+OPEm]/OPEm,SumS[1,-1+OPEm]^2/OPEm,
SumS[2,-1+OPEm],SumS[2,-1+OPEm]/OPEm,SumS[3,-1+OPEm],SumS[1,1,-1+OPEm],
SumS[1,OPEm-1]^2,SumS[1,2,-1+OPEm],SumS[2,1,-1+OPEm],SumS[1,-1+OPEm]^3,
SumS[1,-1+OPEm] SumS[2,-1+OPEm],SumS[1,1,1,-1+OPEm]};


im[z_]:=z->InverseMellin[z,y]


im[OPEm^(-3)]


im[OPEm^(-2)]


im[PolyGamma[0,OPEm]]


im[SumS[1,OPEm-1]]


im[SumS[1,OPEm-1]/(OPEm-1)]


im[SumS[1,OPEm-1]/(OPEm+1)]


im[SumS[1,-1+OPEm]/OPEm^2]


im[SumS[1,-1+OPEm]/OPEm]


im[SumS[1,-1+OPEm]^2/OPEm]


im[SumS[2,OPEm-1]]


im[SumS[2,OPEm-1]/OPEm]


im[SumS[3,OPEm-1]]


im[SumS[1,1,OPEm-1]]


im[SumS[2,1,OPEm-1]]


im[SumS[1,1,1,OPEm-1]]


Clear[im,list];
