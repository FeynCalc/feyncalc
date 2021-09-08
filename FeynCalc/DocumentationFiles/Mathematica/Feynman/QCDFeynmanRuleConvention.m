(* ::Package:: *)

 


(* ::Section:: *)
(*QCDFeynmanRuleConvention*)


(* ::Text:: *)
(*`QCDFeynmanRuleConvention` fixes the sign convention in the QCD Feynman rules for the ghost propagator and the ghost-gluon vertex.This is done by setting the value of `QCDFeynmanRuleConvention[GhostPropagator]` and `QCDFeynmanRuleConvention[GluonGhostVertex]`.*)


(* ::Text:: *)
(*The default values are `1` for both, which corresponds to the convention used in most books. Setting them to `-1` enforces the convention that can be found e.g. in the book "Applications of Perturbative QCD" by R. Field.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonGhostVertex](GluonGhostVertex.md), [GhostPropagator](GhostPropagator.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Enforce the convention as in "Applications of Perturbative QCD" by R. Field.*)


QCDFeynmanRuleConvention[GhostPropagator]=-1;
QCDFeynmanRuleConvention[GluonGhostVertex]=-1;


GHP[p,a,b]//Explicit


GGV[{p,\[Mu],a},{q,\[Nu],b},{k,\[Rho],c}]//Explicit


(* ::Text:: *)
(*Back to the standard convention.*)


QCDFeynmanRuleConvention[GhostPropagator]=1
QCDFeynmanRuleConvention[GluonGhostVertex]=1


GHP[p,a,b]//Explicit


GGV[{p,\[Mu],a},{q,\[Nu],b},{k,\[Rho],c}]//Explicit
