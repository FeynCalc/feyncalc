(* ::Package:: *)

 


(* ::Section:: *)
(*GALR*)


(* ::Text:: *)
(*`GALR[mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\gamma^{\mu}_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALN](GALN.md), [GALP](GALP.md), [GSLP](GSLP.md), [GSLN](GSLN.md), [GSLR](GSLR.md).*)


(* ::Subsection:: *)
(*Examples*)


GALR[\[Mu],n,nb]


StandardForm[GALR[\[Mu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GALR[\[Mu],n,nb].GALP[\[Nu],n,nb]//iracSimplify


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


GALR[\[Mu],n,nb].GALP[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
