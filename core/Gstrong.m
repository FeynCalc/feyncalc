(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The strong coupling constant *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Gstrong`",
             "HighEnergyPhysics`FeynCalc`"];

Gstrong::"usage" =
"Gstrong denotes the strong coupling constant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

  Gstrong /:
   MakeBoxes[Gstrong, TraditionalForm] :=
    SubscriptBox["g","s"]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Gstrong | \n "]];
Null
