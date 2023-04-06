(* ::Package:: *)

 


(* ::Section:: *)
(*FVLPD*)


(* ::Text:: *)
(*`FVLPD[p,mu,n,nb]` denotes the positive component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\frac{1}{2} \bar{n}^{\mu} (p \cdot n)$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


FVLPD[p,\[Mu],n,nb]


StandardForm[FVLPD[p,\[Mu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


FVLPD[p,\[Mu],n,nb] FVLND[q,\[Mu],n,nb]//Contract


FVLPD[p,\[Mu],n,nb] FVLPD[q,\[Mu],n,nb]//Contract


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


FVLPD[p,\[Mu],n,nb] FVLND[q,\[Mu],n,nb]//Contract


FVLPD[p,\[Mu],n,nb]FVLPD[q,\[Mu],n,nb]//Contract


FCClearScalarProducts[]
