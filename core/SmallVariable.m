(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SmallVariable is beautiful *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SmallVariable`",
             "HighEnergyPhysics`FeynCalc`"];

SmallVariable::"usage" =
"SmallVariable[me] is a small (negligible) variable.
This means any mass with head SmallVariable be neglected if it
appears in a sum, but not as an argument of Passarino-Veltman
(PaVe) functions or PropagatorDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SmallVariable[0] = 0;
SmallVariable[x_^pow_] := SmallVariable[x]^pow;

   MakeBoxes[SmallVariable[a_], TraditionalForm] :=
    MakeBoxes[a, TraditionalForm];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SmallVariable | \n "]];
Null
