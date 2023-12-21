(* ::Package:: *)

 


(* ::Section:: *)
(*SPLP*)


(* ::Text:: *)
(*`SPLP[p,q,n,nb]` denotes the positive component in the lightcone decomposition of the scalar product $p \cdot q$  along the vectors `n` and `nb`. It corresponds to $\frac{1}{2} (p \cdot n) (q \cdot \bar{n})$.*)


(* ::Text:: *)
(*If one omits `n` and `nb`, the program will use default vectors specified via `$FCDefaultLightconeVectorN` and `$FCDefaultLightconeVectorNB`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Pair](Pair.md), [FVLN](FVLN.md), [FVLP](FVLP.md), [FVLR](FVLR.md), [SPLN](SPLN.md), [SPLR](SPLR.md), [MTLP](MTLP.md), [MTLN](MTLN.md), [MTLR](MTLR.md).*)


(* ::Subsection:: *)
(*Examples*)


SPLP[p,q,n,nb]


StandardForm[SPLP[p,q,n,nb]//FCI]


(* ::Text:: *)
(*Notice that the properties of `n` and `nb` vectors have to be set by hand before doing the actual computation*)


SPLP[p1+p2+n,q,n,nb]//ExpandScalarProduct


FCClearScalarProducts[]
SP[n]=0;
SP[nb]=0;
SP[n,nb]=2;


SPLP[p1+p2+n,q,n,nb]//ExpandScalarProduct


FCClearScalarProducts[]
