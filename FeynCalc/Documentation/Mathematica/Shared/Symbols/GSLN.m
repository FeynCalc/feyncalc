(* ::Package:: *)

 


(* ::Section:: *)
(*GSLN*)


(* ::Text:: *)
(*`GSLN[p,n,nb]` denotes the negative component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} (n \cdot p) (\gamma \cdot \bar{n})$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALP](GALP.md), [GALN](GALN.md), [GALR](GALR.md), [GSLP](GSLP.md), [GSLR](GSLR.md).*)


(* ::Subsection:: *)
(*Examples*)


GSLN[p,n,nb]


StandardForm[GSLN[p,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GSLN[p,n,nb].GSLN[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


GSLN[p,n,nb].GSLN[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
