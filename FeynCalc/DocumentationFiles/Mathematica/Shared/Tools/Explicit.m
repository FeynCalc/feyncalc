 
(* ::Section:: *)
(* Explicit *)
(* ::Text:: *)
(*Explicit is an option for FieldStrength, GluonVertex, SUNF, and Twist2GluonOperator. If set to True the full form of the operator is inserted. Explicit[exp] inserts explicit expressions of GluonVertex, Twist2GluonOperator etc. in exp. SUNF's are replaced by SUNTrace objects..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*GluonVertex, Twist2GluonOperator.*)



(* ::Subsection:: *)
(* Examples *)



GluonVertex[p,\[Mu],a, q,\[Nu],b,r,\[Rho],c]

Explicit[%]

Twist2GluonOperator[p,\[Mu],a,\[Nu],b]

Explicit[%]

FieldStrength[\[Mu],\[Nu],a]

Explicit[%]
