(* ::Package:: *)

 


(* ::Section:: *)
(*Twist3QuarkOperator*)


(* ::Text:: *)
(*`Twist3QuarkOperator[p]` or `Twist3QuarkOperator[p,_,_]`  yields the  2-quark operator (`p` is momentum in the direction of the fermion number flow).*)


(* ::Text:: *)
(*`Twist3QuarkOperator[{p1,___}, {p2,___}, {p3, mu, a}]` or `Twist3QuarkOperator[p1,_,_,  p2,_,_,  p3,mu,a]` yields the Quark-Quark-Gluon-operator, where `p1` is the incoming quark, `p2` the incoming antiquark and `p3` denotes the (incoming) gluon momentum.*)


(* ::Text:: *)
(*`Twist3QuarkOperator[{p1,___}, {p2,___}, {p3, mu, a}, {p4, nu, b}]` or `Twist3QuarkOperator[p1,_,_,  p2,_,_,  p3,mu,a, p4, nu, b]`  gives the Quark-Quark-Gluon-Gluon-operator. The setting of the option `Polarization` (unpolarized: `0`; polarized: `1`) determines whether the unpolarized or polarized operator is returned.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Twist2QuarkOperator](Twist2QuarkOperator.md), [Twist2GluonOperator](Twist2GluonOperator.md).*)


(* ::Subsection:: *)
(*Examples*)


Twist3QuarkOperator[p]
