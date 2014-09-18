(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Feynman integration *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FCIntegrate`",{"HighEnergyPhysics`FeynCalc`"}];

FCIntegrate::"usage"=
"FCIntegrate is an option of certain Feynman integral related functions. \
It determines which integration function is used to evaluate analytic \
integrals. Possible settings include Integrate, NIntegrate,
(DOT[Integratedx@@#2, #1] &).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FCIntegrate | \n "]];
Null
