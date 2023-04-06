(* ::Package:: *)

 


(* ::Section:: *)
(*GALP*)


(* ::Text:: *)
(*`GALP[mu,n,nb]` denotes the positive component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} \bar{n}^{\mu} (\gamma \cdot n)$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALN](GALN.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLN](GSLN.md), [GSLR](GSLR.md).*)


(* ::Subsection:: *)
(*Examples*)


GALP[\[Mu],n,nb]


StandardForm[GALP[\[Mu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GALP[\[Mu],n,nb].GALP[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


GALP[\[Mu],n,nb].GALP[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
