(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`OPE`",
             "HighEnergyPhysics`FeynCalc`"];

OPE::"usage"= "OPE is used internally in OPE1Loop.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

OPE /: OPE^_Integer?Positive := 0;

   OPE /: MakeBoxes[OPE, TraditionalForm] := "\[CapitalKoppa]"

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPE | \n "]];
Null
