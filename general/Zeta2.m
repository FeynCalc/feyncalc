(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Zeta2 *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Zeta2`",
             "HighEnergyPhysics`FeynCalc`"];

Zeta2::usage=
"Zeta2 denotes Zeta[2]. For convenience every Pi^2 occuring in 
OPEIntDelta is replaced by (6 Zeta2).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Zeta2, ReadProtected];

Zeta2 /: N[Zeta2] = N[Zeta[2]];

   Zeta2 /: 
   MakeBoxes[Zeta2, TraditionalForm] := 
    RowBox[{"\[Zeta]","(",2,")"}];
(*SubscriptBox["\[Zeta]", 2]*)

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Zeta2 | \n "]];
Null
