(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ClearScalarProducts *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 December '98 at 0:06 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: clearing definitions of scalar products *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ClearScalarProducts`",{"HighEnergyPhysics`FeynCalc`"}];

ClearScalarProducts::"usage" =
"ClearScalarProducts removes all user-performed
specific settings for ScalarProduct's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];




ClearScalarProducts := (
DownValues[HighEnergyPhysics`FeynCalc`CoreObjects`Pair] =
HighEnergyPhysics`FeynCalc`CoreObjects`Private`initialDownValues;
DownValues[HighEnergyPhysics`FeynCalc`ScalarProduct`ScalarProduct] =
HighEnergyPhysics`FeynCalc`ScalarProduct`Private`initialDownValues;
UpValues[ScalarProduct] =
HighEnergyPhysics`FeynCalc`ScalarProduct`Private`initialUpValues;
DownValues[HighEnergyPhysics`FeynCalc`CoreObjects`SP] =
HighEnergyPhysics`FeynCalc`CoreObjects`Private`initialSPDownValues;
DownValues[HighEnergyPhysics`FeynCalc`CoreObjects`SPD] =
HighEnergyPhysics`FeynCalc`CoreObjects`Private`initialSPDDownValues;
);


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ClearScalarProducts | \n "]];
Null
