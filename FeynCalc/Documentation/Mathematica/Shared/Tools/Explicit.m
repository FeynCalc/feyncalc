(* ::Package:: *)

 


(* ::Section:: *)
(*Explicit*)


(* ::Text:: *)
(*`Explicit[exp]` inserts explicit expressions of `GluonVertex`, `SUNF` etc. in `exp`.*)


(* ::Text:: *)
(*To rewrite the $SU(N)$ structure constants in terms of traces, please set the corresponding options `SUNF` or `SUND` to `True`.*)


(* ::Text:: *)
(*`Explicit` is also an option for `FieldStrength`, `GluonVertex`, `SUNF`  etc. If set to `True` the full form of the operator is inserted.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md).*)


(* ::Subsection:: *)
(*Examples*)


gv=GluonVertex[p,\[Mu],a, q,\[Nu],b,r,\[Rho],c]


Explicit[gv]


Explicit[gv,SUNF->True]


FieldStrength[\[Mu],\[Nu],a]

Explicit[%]


Explicit[SUNF[a,b,c]]


Explicit[SUNF[a,b,c],SUNF->True]


Explicit[SUND[a,b,c]]


Explicit[SUND[a,b,c],SUND->True]
