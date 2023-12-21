(* ::Package:: *)

 


(* ::Section:: *)
(*SPLPD*)


(* ::Text:: *)
(*`SPLPD[p,q,n,nb]` denotes the positive component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb` in $D$-dimensions. It corresponds to $\frac{1}{2} (p \cdot n) (q \cdot \bar{n})$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [FVLRD](FVLRD.md), [SPLND](SPLND.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


SPLPD[p,q,n,nb]


StandardForm[SPLPD[p,q,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


SPLPD[p1+p2+n,q,n,nb]//ExpandScalarProduct


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


SPLPD[p1+p2+n,q,n,nb]//ExpandScalarProduct


FCClearScalarProducts[]
