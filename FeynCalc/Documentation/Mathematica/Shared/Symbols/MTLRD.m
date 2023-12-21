(* ::Package:: *)

 


(* ::Section:: *)
(*MTLRD*)


(* ::Text:: *)
(*`MTLRD[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $g^{\mu \nu}_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md).*)


(* ::Subsection:: *)
(*Examples*)


MTLRD[\[Mu],\[Nu],n,nb]


MTLRD[\[Mu],\[Nu],n,nb]//FCI//StandardForm


MTLRD[\[Mu],\[Nu],n,nb] FVD[p,\[Mu]]//Contract


MTLRD[\[Mu],\[Nu],n,nb]FVLPD[q,\[Mu],n,nb]//Contract
