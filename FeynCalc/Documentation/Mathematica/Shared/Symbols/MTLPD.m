(* ::Package:: *)

 


(* ::Section:: *)
(*MTLPD*)


(* ::Text:: *)
(*`MTLPD[mu,nu,n,nb]` denotes the positive component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} \bar{n}^{\mu}  n^\nu$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLPD](FVLPD.md), [FVLND](FVLND.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


MTLPD[\[Mu],\[Nu],n,nb]


StandardForm[MTLPD[\[Mu],\[Nu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


MTLPD[\[Mu],\[Nu],n,nb] FVD[p,\[Mu]]//Contract


MTLPD[\[Mu],\[Nu],n,nb] FVD[p,\[Nu]]//Contract


MTLPD[\[Mu],\[Nu],n,nb] FVD[n,\[Nu]]//Contract


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


MTLPD[\[Mu],\[Nu],n,nb] FVD[n,\[Nu]]//Contract


FCClearScalarProducts[]
