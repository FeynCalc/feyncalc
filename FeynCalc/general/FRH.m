(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FRH *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: FRH[x_] := FixedPoint[ReleaseHold, x] *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`FRH`",
             {"HighEnergyPhysics`FeynCalc`"}];

FRH::"usage" =
"FRH[exp_] := FixedPoint[ReleaseHold, exp], i.e., FRH removes all
HoldForm and Hold in exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FRH[x_] := FixedPoint[ReleaseHold, x];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FRH | \n "]];
Null
