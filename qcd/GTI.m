(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GTI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`qcd`GTI`",
             "HighEnergyPhysics`FeynCalc`"];

GTI::"usage"= "GTI is like RHI, but with no functional properties.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Momentum];

Options[GTI] = {Momentum -> Global`p};

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GTI | \n "]];
Null
