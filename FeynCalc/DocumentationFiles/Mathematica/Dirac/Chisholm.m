(* ::Package:: *)

 


(* ::Section:: *)
(*Chisholm*)


(* ::Text:: *)
(*`Chisholm[exp]` substitutes products of three Dirac matrices or slashes by the Chisholm identity.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


GA[\[Mu],\[Nu],\[Rho]]
EpsChisholm[%]


(* ::Text:: *)
(*Notice that the output contains dummy indices.*)


GA[\[Alpha],\[Beta],\[Mu],\[Nu]]
Chisholm[%]


(* ::Text:: *)
(*Dummy Lorentz indices may also appear as FCGV.*)


SpinorVBar[p1,m1] . GA[\[Alpha],\[Beta],\[Mu],\[Nu]] . SpinorU[p2,m2]
Chisholm[%]


(* ::Text:: *)
(*Chisholm only works with Dirac matrices in $4$ dimensions, $D$-dimensional objects are ignored.*)


Chisholm[GAD[\[Mu],\[Nu],\[Rho]]]


Chisholm[GA[\[Alpha],\[Beta],\[Mu]]] . Chisholm[GA[\[Alpha],\[Beta],\[Mu]]]
DiracSimplify[%]


Chisholm[GA[\[Alpha],\[Beta],\[Mu],\[Nu]]] . Chisholm[GA[\[Alpha],\[Beta],\[Mu],\[Nu]]]
DiracSimplify[%]


GS[p,q,r]
Chisholm[%]


GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]]
Chisholm[%]


(* ::Text:: *)
(*Check the equality of the expressions before and after applying `Chisholm`.*)


DiracSimplify[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]] . GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]]]


DiracSimplify[Chisholm[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]]] . Chisholm[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]]]]


DiracReduce[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]] . Chisholm[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa]]]]


(* ::Text:: *)
(*Older FeynCalc versions had a function called `Chisholm2` that acted on expressions like $\gamma^{\mu} \gamma^{\nu} \gamma^5$. This functionality is now part of `Chisholm` and can be activated by setting the option `Mode` to `2`.*)


GA[\[Mu],\[Nu],5]
Chisholm[%,Mode->2]



