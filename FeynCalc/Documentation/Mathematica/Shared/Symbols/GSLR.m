(* ::Package:: *)

 


(* ::Section:: *)
(*GSLR*)


(* ::Text:: *)
(*`GSLR[p,n,nb]` denotes the perpendicular component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`. It corresponds to $(\gamma \cdot \p)_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALN](GALN.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLN](GSLN.md).*)


(* ::Subsection:: *)
(*Examples*)


GSLR[p,n,nb]


StandardForm[GSLR[p,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GSLR[p,n,nb] . GSLP[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


GSLR[p,n,nb] . GSLP[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
