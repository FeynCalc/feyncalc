(* :Title: MLimit *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 2 May 2001 at 17:02 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`MLimit`",
             "HighEnergyPhysics`FeynCalc`"];

MLimit::"usage"=
"MLimit[expr, {lims}] takes multiple limits of expr using the limits lims.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Options[MLimit] = {Limit -> Limit};

MLimit[x_, l_List, opts___?OptionQ] :=
   Fold[(Limit/.Flatten[{opts}]/.Options[MLimit])[#1, Flatten[{##2}][[1]]]&,
        x, l];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MLimit | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
