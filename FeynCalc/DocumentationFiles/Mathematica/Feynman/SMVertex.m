(* ::Package:: *)

 


(* ::Section:: *)
(*SMVertex*)


(* ::Text:: *)
(*`SMVertex` is a library of SM vertices. Currently it implements only few vertices and is not really useful.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SMP](SMP.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This is the $\gamma W W$ vertex (all momenta ingoing)*)


SMVertex["AWW",p,\[Mu], q,\[Nu], k,\[Rho]]


(* ::Text:: *)
(*This is the $HHH$-coupling*)


SMVertex["HHH"]


(* ::Text:: *)
(*This is the $H e$-coupling*)


SMVertex["eeH"]
