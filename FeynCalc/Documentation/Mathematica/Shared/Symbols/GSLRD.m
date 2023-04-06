(* ::Package:: *)

 


(* ::Section:: *)
(*GSLRD*)


(* ::Text:: *)
(*`GSLRD[p,n,nb]` denotes the perpendicular component in the lightcone decomposition of the slashed Dirac matrix $(\gamma \cdot p)$  along the vectors `n` and `nb`  in $D$ dimensions. It corresponds to $(\gamma \cdot \p)_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GALPD](GALPD.md), [GALND](GALND.md), [GALRD](GALRD.md), [GSLPD](GSLPD.md), [GSLND](GSLND.md).*)


(* ::Subsection:: *)
(*Examples*)


GSLRD[p,n,nb]


StandardForm[GSLRD[p,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


GSLRD[p,n,nb] . GSLPD[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


GSLRD[p,n,nb] . GSLPD[q,n,nb]//DiracSimplify


FCClearScalarProducts[]
