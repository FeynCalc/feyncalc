(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NumericalFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: NumericalFactor take out a numerical factor *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`general`NumericalFactor`",
             "HighEnergyPhysics`FeynCalc`"];

NumericalFactor::"usage" = "NumericalFactor[expr]
gives the overall numerical factor of expr.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

NumericalFactor[a___ /; Length[{a}] =!=1] :=
soso /; Message[NumericalFactor::argrx, NumericalFactor, Length[{a}], 1];
NumericalFactor[x_]:= If[NumberQ[x], x, If[Head[x] === Times,
                         If[NumberQ[First[x]], First[x], 1], 1]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NumericalFactor | \n "]];
Null