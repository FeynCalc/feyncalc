(* ::Package:: *)

 


(* ::Section:: *)
(*MTLN*)


(* ::Text:: *)
(*`MTLN[mu,nu,n,nb]` denotes the positive component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} n^{\mu}  \bar{n}^\nu$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLP](FVLP.md), [FVLN](FVLN.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLR](MTLR.md).*)


(* ::Subsection:: *)
(*Examples*)


MTLN[\[Mu],\[Nu],n,nb]


StandardForm[MTLN[\[Mu],\[Nu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


MTLN[\[Mu],\[Nu],n,nb] FV[p,\[Mu]]//Contract


MTLN[\[Mu],\[Nu],n,nb] FV[p,\[Nu]]//Contract


MTLN[\[Mu],\[Nu],n,nb] FV[n,\[Nu]]//Contract


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


MTLN[\[Mu],\[Nu],n,nb] FV[n,\[Nu]]//Contract


FCClearScalarProducts[]
