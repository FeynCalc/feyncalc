(* ::Package:: *)

 


(* ::Section:: *)
(*SPLR*)


(* ::Text:: *)
(*`SPLR[p,q,n,nb]` denotes the perpendicular component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb`. It corresponds to $(p \cdot q)_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [FVLR](FVLR.md), [SPLP](SPLP.md), [SPLN](SPLN.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).*)


(* ::Subsection:: *)
(*Examples*)


SPLR[p,q,n,nb]


StandardForm[SPLR[p,q,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


SPLR[p1+p2,q1+q2,n,nb]//FCI//ExpandScalarProduct


SPLR[p1+p2+n,q,n,nb]//FCI//ExpandScalarProduct
