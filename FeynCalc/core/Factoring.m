(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Factoring is an option for several functions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Factoring`",{"HighEnergyPhysics`FeynCalc`"}];

Factoring::"usage" = "Factoring is an option for Collect2, Contract,
Tr and more functions. If set to True, the result will be
factored, using Factor2. If set to any function f, this function
will be used.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Factoring | \n "]];
Null
