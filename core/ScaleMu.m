(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dimensional regularization scale *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ScaleMu`",
             "HighEnergyPhysics`FeynCalc`"];

ScaleMu::"usage"= "ScaleMu is the mass scale used for dimensional \
regularization of loop integrals";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ScaleMu /:
MakeBoxes[ScaleMu, TraditionalForm] := "\[Mu]";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScaleMu | \n "]];
Null
