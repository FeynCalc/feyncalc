(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`CommutatorExplicit`",{"HighEnergyPhysics`FeynCalc`"}];

CommutatorExplicit::"usage"=
"CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator \
in exp by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[
AntiCommutator,
Commutator
];

CommutatorExplicit[exp_] := exp /.
   {Commutator :> ((DOT[#1, #2] - DOT[#2, #1])&),
    AntiCommutator :> ((DOT[#1, #2] + DOT[#2, #1])&)
   };

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CommutatorExplicit | \n "]];
Null
