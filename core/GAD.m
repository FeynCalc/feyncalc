(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`GAD`",{"HighEnergyPhysics`FeynCalc`"}];

GAD::"usage"=
"GAD[mu] can be used as input for a D-dimensional gamma_mu and is
transformed into DiracMatrix[mu, Dimension->D] by FeynCalcInternal.";



(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext["DeclareNonCommutative"][GAD];

GAD[DOT[x_,y__]] := Map[GAD, DOT[x,y]];
GAD[x_, y__] := DOT @@ Map[GAD,{x,y}];

GAD /:
  MakeBoxes[ GAD[x_], TraditionalForm ] := If[$Covariant,
             SubscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]],
             SuperscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]]
                                             ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GAD | \n "]];
Null
