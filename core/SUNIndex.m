(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Head for SUN-Indices *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SUNIndex`",
             "HighEnergyPhysics`FeynCalc`"];

SUNIndex::"usage"=
"SUNIndex[a] is an SU(N) index. If the argument is an integer
SUNIndex[a] turns into ExplicitSUNIndex[a].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ExplicitSUNIndex = MakeContext["ExplicitSUNIndex"];

SetAttributes[SUNIndex, {Constant, Flat, OneIdentity}];

SUNIndex[i_Integer]:= ExplicitSUNIndex[i];

   SUNIndex /:
   MakeBoxes[ SUNIndex[p_], TraditionalForm
            ] := ToBoxes[p, TraditionalForm];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNIndex | \n "]];
Null
