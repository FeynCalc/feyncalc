(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`InitialFunction`",
             "HighEnergyPhysics`FeynCalc`"];

InitialFunction::"usage"= 
"InitialFunction is an option of FeynRule the setting of which is applied to \
the first argument of FeynRule before anything else";

EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

If[$VeryVerbose > 0,WriteString["stdout", "InitialFunction | \n "]];
Null
