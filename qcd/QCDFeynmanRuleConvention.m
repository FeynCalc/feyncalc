(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDFeynmanRuleConvention *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: added Sept. 28th 2003 in order to adapt to various conventions of
factors of I etc. *)
(* ------------------------------------------------------------------------ *)

(* :Summary: QCDFeynmanRuleConvention *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`QCDFeynmanRuleConvention`",
             "HighEnergyPhysics`FeynCalc`"];

QCDFeynmanRuleConvention::"usage" =
"QCDFeynmanRuleConvention[GluonPropagator], 
QCDFeynmanRuleConvention[GluonGhostVertex], etc. can be set to 
different values. The default is the Itzikson-Zuber convention. ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
GluonPropagator,
GluonVertex,
GluonGhostVertex,
GhostPropagator];

QCDFeynmanRuleConvention[_] = 1;
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QCDFeynmanRuleConvention | \n "]];
Null
