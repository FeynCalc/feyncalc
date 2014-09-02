(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Extension of FreeQ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FreeQ2`",{"HighEnergyPhysics`FeynCalc`"}];

FreeQ2::"usage" =
"FreeQ2[expr, {form1, form2, ...}] yields True if expr does not
contain any occurence of form1, form2, ... and False otherwise.
FreeQ2[expr, form] is the same as FreeQ[expr, form].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FreeQ2[_,{}]          := True;
FreeQ2[x_, y_]        := FreeQ[x, y] /; Head[y] =!= List;
FreeQ2[x_, {y_}]      := FreeQ[x, y];
FreeQ2[x_, {y_, z__}] := If[FreeQ[x, y], FreeQ2[x, {z}], False];
(* this is eventually slower ...
FreeQ2[x_, {y_, z__}] := FreeQ[x, Alternatives@@{y,z}];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FreeQ2 | \n "]];
Null
