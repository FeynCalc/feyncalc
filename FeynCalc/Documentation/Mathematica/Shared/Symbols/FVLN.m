(* ::Package:: *)

 


(* ::Section:: *)
(*FVLN*)


(* ::Text:: *)
(*`FVLN[p,mu,n,nb]` denotes the positive component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} n^{\mu} (p \cdot \bar{n})$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLP](FVLP.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).*)


(* ::Subsection:: *)
(*Examples*)


FVLN[p,\[Mu],n,nb]


StandardForm[FVLN[p,\[Mu],n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


FVLP[p,\[Mu],n,nb]FVLN[q,\[Mu],n,nb]//Contract


FVLP[p,\[Mu],n,nb]FVLP[q,\[Mu],n,nb]//Contract


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


FVLP[p,\[Mu],n,nb] FVLN[q,\[Mu],n,nb]//Contract


FVLP[p,\[Mu],n,nb] FVLP[q,\[Mu],n,nb]//Contract


FCClearScalarProducts[]
