(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallDelta is some small delta *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SmallDelta`",
             "HighEnergyPhysics`FeynCalc`"];

SmallDelta::"usage" = "SmallDelta denotes some small positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   SmallDelta /:
   MakeBoxes[SmallDelta, TraditionalForm] := "\[Delta]"

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SmallDelta | \n "]];
Null
