(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`KeepOnly`",
             "HighEnergyPhysics`FeynCalc`"];

KeepOnly::"usage"=
"KeepOnly is an option of OneLoop.
It may be set to B0, C0, D0 keeping only the corresponding
coefficients. The default setting is False. If KeepOnly is set
to {} then the part of the amplitude which is not coefficient
of B0, C0, D0 is kept.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "KeepOnly | \n "]];
Null
