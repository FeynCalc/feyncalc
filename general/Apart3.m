(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Apart3 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Apart3`",
             "HighEnergyPhysics`FeynCalc`"];

Apart3::usage=
"Apart3[expr, x] is equivalent to
Map2[Factor2, Collect2[Apart1[expr,x],x]].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Apart1, Collect2, Factor2, Map2];

Apart3[expr_, x_] := Map2[Factor2, Collect2[Apart1[expr,x],x]];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Apart3 | \n "]];
Null
