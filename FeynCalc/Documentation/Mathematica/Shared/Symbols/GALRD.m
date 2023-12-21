(* ::Package:: *)

 


(* ::Section:: *)
(*GALRD*)


(* ::Text:: *)
(*`GALRD[mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\gamma^{\mu}_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALND](GALND.md), [GALPD](GALPD.md), [GSLPD](GSLPD.md), [GSLND](GSLND.md), [GSLRD](GSLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


GALRD[\[Mu],n,nb]


StandardForm[GALRD[\[Mu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GALRD[\[Mu],n,nb].GALPD[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


GALRD[\[Mu],n,nb].GALPD[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
