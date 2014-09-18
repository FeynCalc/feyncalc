(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FinalSubstitutions`",{"HighEnergyPhysics`FeynCalc`"}];

FinalSubstitutions::"usage" =
"FinalSubstitutions is an option for OneLoop, OneLoopSum,
Write2, FeynCalcExternal and FeynCalcInternal. All substitutions indicated hereby are done at the
end of the calculation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FinalSubstitutions | \n "]];
Null
