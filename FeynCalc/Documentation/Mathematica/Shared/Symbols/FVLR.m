(* ::Package:: *)

 


(* ::Section:: *)
(*FVLR*)


(* ::Text:: *)
(*`FVLR[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the Lorentz vector $p^{\mu }$  along the vectors `n` and `nb`. It corresponds to $p^{\mu }_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).*)


(* ::Subsection:: *)
(*Examples*)


FVLR[p,\[Mu],n,nb]


FVLR[p,\[Mu],n,nb]//FCI//StandardForm


FVLR[p,\[Mu],n,nb] FVLR[q,\[Mu],n,nb]//Contract


FVLR[p,\[Mu],n,nb].FVLP[q,\[Mu],n,nb]//Contract
