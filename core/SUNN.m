(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SUNN = the N of SU(N) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SUNN`",
             "HighEnergyPhysics`FeynCalc`"];

SUNN::"usage" =
"SUNN denotes the number of colors.
Trick[SUNDelta[a, a]] yields (SUNN^2 -1).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(* add maybe later something to convert SUNN^2 -> CA, CF *)

HighEnergyPhysics`FeynCalc`SUNN`SUNN /:
   MakeBoxes[ HighEnergyPhysics`FeynCalc`SUNN`SUNN,
              TraditionalForm
            ] := "N";

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNN | \n "]];
Null
