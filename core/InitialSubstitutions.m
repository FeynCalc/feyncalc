(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: option for several functions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`InitialSubstitutions`",
             "HighEnergyPhysics`FeynCalc`"];

InitialSubstitutions::"usage" =
"InitialSubstitutions is an option for OneLoop and OneLoopSum
and Write2. All substitutions indicated hereby are done at the
end of the calculation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "InitialSubstitutions | \n "]];
Null
