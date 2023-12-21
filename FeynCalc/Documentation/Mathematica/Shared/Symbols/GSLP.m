(* ::Package:: *)

 


(* ::Section:: *)
(*GSLP*)


(* ::Text:: *)
(*`GSLP[p,n,nb]` denotes the positive component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} (\bar{n} \cdot p) (\gamma \cdot n)$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALN](GALN.md), [GALR](GALR.md), [GSLN](GSLN.md), [GSLR](GSLR.md).*)


(* ::Subsection:: *)
(*Examples*)


GSLP[p,n,nb]


StandardForm[GSLP[p,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GSLP[p,n,nb].GSLP[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


GSLP[p,n,nb].GSLP[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
