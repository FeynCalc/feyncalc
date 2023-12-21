(* ::Package:: *)

 


(* ::Section:: *)
(*MTLND*)


(* ::Text:: *)
(*`MTLND[mu,nu,n,nb]` denotes the positive component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`in $D$ dimensions. It corresponds to $\frac{1}{2} n^{\mu}  \bar{n}^\nu$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLPD](FVLPD.md), [FVLND](FVLND.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


MTLND[\[Mu],\[Nu],n,nb]


StandardForm[MTLND[\[Mu],\[Nu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


MTLND[\[Mu],\[Nu],n,nb] FVD[p,\[Mu]]//Contract


MTLND[\[Mu],\[Nu],n,nb] FVD[p,\[Nu]]//Contract


MTLND[\[Mu],\[Nu],n,nb] FVD[n,\[Nu]]//Contract


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


MTLND[\[Mu],\[Nu],n,nb] FVD[n,\[Nu]]//Contract


FCClearScalarProducts[]
