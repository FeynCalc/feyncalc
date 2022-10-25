(* ::Package:: *)

 


(* ::Section:: *)
(*Explicit*)


(* ::Text:: *)
(*`Explicit[exp]` inserts explicit expressions of `GluonVertex`, `Twist2GluonOperator`, `SUNF` etc. in `exp`.*)


(* ::Text:: *)
(*To rewrite the $SU(N)$ structure constants in terms of traces, please set the corresponding options `SUNF` or `SUND` to `True`.*)


(* ::Text:: *)
(*The color traces are left untouched unless the option `SUNTrace` is set to `True`. In this case they will be rewritten in terms of structure constants.*)


(* ::Text:: *)
(*`Explicit` is also an option for `FieldStrength`, `GluonVertex`, `SUNF`,  `Twist2GluonOperator` etc. If set to `True` the full form of the operator is inserted.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md), [Twist2GluonOperator](Twist2GluonOperator.md).*)


(* ::Subsection:: *)
(*Examples*)


gv=GluonVertex[p,\[Mu],a, q,\[Nu],b,r,\[Rho],c]


Explicit[gv]


Explicit[gv,SUNF->True]


Twist2GluonOperator[p,\[Mu],a,\[Nu],b]

Explicit[%]


FieldStrength[\[Mu],\[Nu],a]

Explicit[%]


Explicit[SUNF[a,b,c]]


Explicit[SUNF[a,b,c],SUNF->True]


Explicit[SUND[a,b,c]]


Explicit[SUND[a,b,c],SUND->True]


Explicit[SUNTrace[SUNT[a,b,c]]]


Explicit[SUNTrace[SUNT[a,b,c]],SUNTrace->True]
