(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Collecting *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Collecting`",
             "HighEnergyPhysics`FeynCalc`"];

Collecting::"usage" =
"Collecting is an option of Contract2, ScalarProductCancel, SquareAmplitude, \
Series2, TID and related functions. Setting it to True will trigger \
some kind of collecting of the result.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Collecting | \n "]];
Null
