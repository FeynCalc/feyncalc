(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: EpsilonOrder *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`EpsilonOrder`",{"HighEnergyPhysics`FeynCalc`"}];

EpsilonOrder::"usage" =
"EpsilonOrder is an option of OPEIntegrateDelta and RHI. The setting
determines the order n (Epsilon^n) which should be kept.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsilonOrder | \n "]];
Null
