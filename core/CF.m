(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: CF *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`CF`",
             "HighEnergyPhysics`FeynCalc`"];

CF::"usage"=
"CF is one of the Casimir operator eigenvalues of SU(N); CF = (N^2-1)/(2 N)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CF /:
   MakeBoxes[
             CF, TraditionalForm
            ] := SubscriptBox["C", "F"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CF | \n "]];
Null
