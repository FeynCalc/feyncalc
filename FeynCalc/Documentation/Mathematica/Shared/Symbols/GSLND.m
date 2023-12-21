(* ::Package:: *)

 


(* ::Section:: *)
(*GSLND*)


(* ::Text:: *)
(*`GSLND[p,n,nb]` denotes the negative component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb` in $D$ dimensions. It corresponds to $\frac{1}{2} (n \cdot p) (\gamma \cdot \bar{n})$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALPD](GALPD.md), [GALND](GALND.md), [GALRD](GALRD.md), [GSLPD](GSLPD.md), [GSLRD](GSLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


GSLND[p,n,nb]


StandardForm[GSLND[p,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GSLND[p,n,nb].GSLND[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


GSLND[p,n,nb].GSLND[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
