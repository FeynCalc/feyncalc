(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Rename *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Rename`",
             "HighEnergyPhysics`FeynCalc`"];

Rename::"usage" =
"Rename is an option for Contract. If set to True,
dummy indices in Eps are renamed, using $MU[i].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Rename | \n "]];
Null
