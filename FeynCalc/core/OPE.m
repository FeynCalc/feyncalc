(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`OPE`",{"HighEnergyPhysics`FeynCalc`"}];

OPE::"usage"= "OPE is a convenience variable to separate OPE insertions. 
OPE is also an option of several input functions like GluonPropagator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

OPE /: OPE^_Integer?Positive := 0;

   OPE /: MakeBoxes[OPE, TraditionalForm] := "\[CapitalOmega]"

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPE | \n "]];
Null
