(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEk*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEk`",{"HighEnergyPhysics`FeynCalc`"}];

OPEk::"usage"= "OPEk is an dummy index in OPESum.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

PositiveInteger = MakeContext["CoreObjects","PositiveInteger"];
MakeContext[DataType];
DataType[OPEk, PositiveInteger] = True;

   OPEk /:
   MakeBoxes[OPEk ,TraditionalForm] := "k";


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEk | \n "]];
Null
