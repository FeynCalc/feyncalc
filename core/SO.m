(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SO`",{"HighEnergyPhysics`FeynCalc`"}];

SO::"usage"=
"SO[q] is the four-dimensional scalar product of OPEDelta with q.
It is transformed into
Pair[Momentum[q], Momentum[OPEDelta] by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   SO /:
   MakeBoxes[SO[x_],TraditionalForm] :=
    If[Head[x] =!= Plus,
       TBox["\[CapitalDelta]",  "\[CenterDot]", x],
       TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
      ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SO | \n "]];
Null
