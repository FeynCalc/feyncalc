(* ::Package:: *)

 


(* ::Section:: *)
(*FVLRD*)


(* ::Text:: *)
(*`FVLRD[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $p^{\mu }_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


FVLRD[p,\[Mu],n,nb]


FVLRD[p,\[Mu],n,nb]//FCI//StandardForm


FVLRD[p,\[Mu],n,nb] FVLRD[q,\[Mu],n,nb]//Contract


FVLRD[p,\[Mu],n,nb] . FVLPD[q,\[Mu],n,nb]//Contract
