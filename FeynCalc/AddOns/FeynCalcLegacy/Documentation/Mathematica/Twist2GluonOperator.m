(* ::Package:: *)

 


(* ::Section:: *)
(*Twist2GluonOperator*)


(* ::Text:: *)
(*`Twist2GluonOperator[{p, mu, a}, {nu, b}]` or `Twist2GluonOperator[p, {mu, a}, {nu, b}]` or `Twist2GluonOperator[p, mu,a, nu,b]` yields the 2-gluon operator (`p` is ingoing momentum corresponding to Lorentz index `mu`).*)


(* ::Text:: *)
(*`Twist2GluonOperator[{p,mu,a}, {q,nu,b}, {k,la,c}]` or `Twist2GluonOperator[ p,mu,a , q,nu,b , k,la,c]` gives the 3-gluon operator.*)


(* ::Text:: *)
(*`Twist2GluonOperator[{p,mu,a}, {q,nu,b}, {k,la,c}, {s,si,d}]` or `Twist2GluonOperator[p,mu,a , q,nu,b , k,la,c , s,si,d]` yields the 4-Gluon operator.*)


(* ::Text:: *)
(*The dimension is determined by the option `Dimension`. The setting of the option `Polarization` (unpolarized: `0`; polarized: `1`) determines whether the unpolarized or polarized Feynman rule is returned.*)


(* ::Text:: *)
(*With the setting `Explicit` set to `False` the color-structure and the (`1+(-1)^OPEm`) (for polarized: `(1-(-1)^OPEm)`) is extracted and the color indices are omitted in the arguments of `Twist2GluonOperator`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Twist2QuarkOperator](Twist2QuarkOperator.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*The setting All for Explicit performs the sums.*)


Twist2GluonOperator[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c}, Polarization -> 1, Explicit->All]
