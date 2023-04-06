(* ::Package:: *)

 


(* ::Section:: *)
(*SPLND*)


(* ::Text:: *)
(*`SPLND[p,q,n,nb]` denotes the negative component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb` in $D$-dimensions. It corresponds to $\frac{1}{2} (p \cdot \bar{n}) (q \cdot n)$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLND](FVLND.md), [FVLPD](FVLPD.md), [FVLRD](FVLRD.md), [SPLPD](SPLPD.md), [SPLRD](SPLRD.md), [MTLPD](MTLPD.md), [MTLND](MTLND.md), [MTLRD](MTLRD.md).*)


(* ::Subsection:: *)
(*Examples*)


SPLND[p,q,n,nb]


StandardForm[SPLND[p,q,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


SPLND[p1+p2+n,q,n,nb]//ExpandScalarProduct


FCClearScalarProducts[]
SPD[n]=0;
SPD[nb]=0;
SPD[n,nb]=2;


SPLND[p1+p2+n,q,n,nb]//ExpandScalarProduct


FCClearScalarProducts[]
