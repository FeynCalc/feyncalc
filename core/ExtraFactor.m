(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExtraFactor *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ExtraFactor`",{"HighEnergyPhysics`FeynCalc`"}];

ExtraFactor::"usage"=
"ExtraFactor is an option for SquareAmplitude and FermionSpinSum.
The setting ExtraFactor -> fa  multiplies the whole amplitude with the
factor fa before squaring.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExtraFactor | \n "]];
Null
