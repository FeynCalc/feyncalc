(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`GA`",
             "HighEnergyPhysics`FeynCalc`"];

GA::"usage"=
"GA[mu] can be used as input for gamma_mu and is
transformed into DiracMatrix[mu] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext["DeclareNonCommutative"][GA];

GA[DOT[x_,y__]] := Map[GA,DOT[x,y]];
GA[x_, y__] := DOT @@ Map[GA,{x,y}];

GA /:
  MakeBoxes[ GA[x_], TraditionalForm ] := If[$Covariant,
                   SubscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]],
                   SuperscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]]
                                            ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GA | \n "]];
Null
