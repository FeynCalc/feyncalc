(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`AntiQuarkField`",{"HighEnergyPhysics`FeynCalc`"}];

AntiQuarkField::"usage" =
"AntiQuarkField is the name of a fermionic field.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

AntiQuarkField /: MakeBoxes[AntiQuarkField, TraditionalForm] :=
  OverscriptBox["\[Psi]","_"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "AntiQuarkField | \n "]];
Null
