(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MT`",
             "HighEnergyPhysics`FeynCalc`"];

MT::"usage"=
"MT[mu, nu] is the metric tensor in 4 dimensions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci := fci  = MakeContext["FeynCalcInternal"];

MakeContext[ Momentum, SP, SPD];

MT[Momentum[a_], Momentum[b_]] := SP[a,b];
MT[Momentum[a_,D], Momentum[b_,D]] := SPD[a,b];


   MT /: MakeBoxes[ MT[x_,y__], TraditionalForm ] :=
   SuperscriptBox["g", HighEnergyPhysics`FeynCalc`Tbox[x,y]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MT | \n "]];
Null
