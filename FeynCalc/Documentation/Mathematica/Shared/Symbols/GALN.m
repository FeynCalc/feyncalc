(* ::Package:: *)

 


(* ::Section:: *)
(*GALN*)


(* ::Text:: *)
(*`GALN[mu,n,nb]` denotes the negative component in the lightcone decomposition of the Dirac matrix $\gamma^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} n^{\mu} (\gamma \cdot \bar{n})$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLN](GSLN.md), [GSLR](GSLR.md).*)


(* ::Subsection:: *)
(*Examples*)


GALN[\[Mu],n,nb]


StandardForm[GALN[\[Mu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GALN[\[Mu],n,nb].GALN[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


GALN[\[Mu],n,nb].GALN[\[Nu],n,nb]//DiracSimplify


FCClearScalarProducts[]
