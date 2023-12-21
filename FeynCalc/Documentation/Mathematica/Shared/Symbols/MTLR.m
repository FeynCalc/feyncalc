(* ::Package:: *)

 


(* ::Section:: *)
(*MTLR*)


(* ::Text:: *)
(*`MTLR[p,mu,n,nb]` denotes the perpendicular component in the lightcone decomposition of the metric tensor $g^{\mu \nu}$  along the vectors `n` and `nb`. It corresponds to $g^{\mu \nu}_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md).*)


(* ::Subsection:: *)
(*Examples*)


MTLR[\[Mu],\[Nu],n,nb]


MTLR[\[Mu],\[Nu],n,nb]//FCI//StandardForm


MTLR[\[Mu],\[Nu],n,nb] FV[p,\[Mu]]//Contract


MTLR[\[Mu],\[Nu],n,nb]FVLP[q,\[Mu],n,nb]//Contract
