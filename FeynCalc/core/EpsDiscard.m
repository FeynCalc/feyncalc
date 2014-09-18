(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`EpsDiscard`",{"HighEnergyPhysics`FeynCalc`"}];


EpsDiscard::"usage"=
"EpsDiscard is an option for FeynCalc2FORM and SquareAmplitude.
If set to True all
Levi-Civita tensors are replaced by 0 after contraction.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsDiscard | \n "]];
Null
