(* ::Package:: *)

 


(* ::Section:: *)
(*Explicit*)


(* ::Text:: *)
(*`Explicit` is an option for `FieldStrength`, `GluonVertex`, `SUNF`, and `Twist2GluonOperator`. If set to `True` the full form of the operator is inserted.*)


(* ::Text:: *)
(*`Explicit[exp]` inserts explicit expressions of `GluonVertex`, `Twist2GluonOperator` etc. in `exp`. `SUNF`s are replaced by `SUNTrace` objects.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md), [Twist2GluonOperator](Twist2GluonOperator.md).*)


(* ::Subsection:: *)
(*Examples*)


GluonVertex[p,\[Mu],a, q,\[Nu],b,r,\[Rho],c]
Explicit[%]


Twist2GluonOperator[p,\[Mu],a,\[Nu],b]
Explicit[%]


FieldStrength[\[Mu],\[Nu],a]
Explicit[%]



