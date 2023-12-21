(* ::Package:: *)

 


(* ::Section:: *)
(*SPLRD*)


(* ::Text:: *)
(*`SPLRD[p,q,n,nb]` denotes the perpendicular component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb`. It corresponds to $(p \cdot q)_{\perp}$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLND](SPLND.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


SPLRD[p,q,n,nb]


StandardForm[SPLRD[p,q,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


SPLRD[p1+p2,q1+q2,n,nb]//FCI//ExpandScalarProduct


SPLRD[p1+p2+n,q,n,nb]//FCI//ExpandScalarProduct
