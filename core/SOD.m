(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SOD`",
             "HighEnergyPhysics`FeynCalc`"];

SOD::"usage"= "SOD[q] stands for the D-dimensional scalar product of
OPEDelta with q. SOD[q] is transformed into Pair[Momentum[OPEDelta,D],
Momentum[q,D]] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   SOD /:
   MakeBoxes[SOD[x_],TraditionalForm] :=
    If[Head[x] =!= Plus,
       TBox["\[CapitalDelta]",  "\[CenterDot]",x],
       TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
      ];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SOD | \n "]];
Null
