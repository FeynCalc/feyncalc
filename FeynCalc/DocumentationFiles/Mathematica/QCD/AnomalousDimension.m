(* ::Package:: *)

 


(* ::Section:: *)
(*AnomalousDimension*)


(* ::Text:: *)
(*`AnomalousDimension[name]` is a database of anomalous dimensions of twist 2 operators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[See also: SplittingFunction](See also: SplittingFunction), [SumS](SumS), [SumT](SumT).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Polarized case:*)


SetOptions[AnomalousDimension,Polarization->1]


(* ::Text:: *)
(*$\gamma _{NS,qq }^{(0) }$ polarized:*)


AnomalousDimension[gnsqq0]


(* ::Text:: *)
(*$\gamma _{S,qg }^{(0)}$ polarized:*)


AnomalousDimension[gsqg0]


(* ::Text:: *)
(*$\gamma _{S,gq }^{(0)}$polarized:*)


AnomalousDimension[gsgq0]


(* ::Text:: *)
(*$\gamma _{S,gg}^{(0)}$ polarized:*)


AnomalousDimension[gsgg0]


(* ::Text:: *)
(*$\gamma _{PS,qq}^{(0)}$ polarized:*)


AnomalousDimension[gpsqq1]


(* ::Text:: *)
(*$\gamma _{NS,qq }^{(1)}$ polarized:*)


AnomalousDimension[gnsqq1]


(* ::Text:: *)
(*$\gamma _{S,qg }^{(1)}$ polarized:*)


AnomalousDimension[gsqg1]


(* ::Text:: *)
(*$\gamma _{S,gq }^{(1)}$ polarized:*)


AnomalousDimension[gsgq1]


(* ::Text:: *)
(*$\gamma _{S,gg }^{(1)}$ polarized:*)


AnomalousDimension[gsgg1]


(* ::Text:: *)
(*$\gamma _{S,gg }^{(1)}$ polarized (different representation):*)


AnomalousDimension[GSGG1]


(* ::Text:: *)
(*Check that all odd moments give the same for the two representations of $\gamma _{S,gg }^{(1)}$:*)


Table[%-%%/.OPEm->ij,{ij,1,17,2}]
