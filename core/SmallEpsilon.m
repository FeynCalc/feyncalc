(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallEpsilon is some small epsilon *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SmallEpsilon`",
             "HighEnergyPhysics`FeynCalc`"];

SmallEpsilon::"usage" = "SmallEpsilon denotes some small
positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*
SmallEpsilon /: SmallEpsilon _ = 0;
SmallEpsilon /: SmallEpsilon^_Integer?Positive = 0;
*)

SmallEpsilon /:
MakeBoxes[SmallEpsilon, TraditionalForm] := "\[Epsilon]";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SmallEpsilon | \n "]];
Null
