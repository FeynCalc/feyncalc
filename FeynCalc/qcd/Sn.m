(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Sn*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the s_n for  OPEInt *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`Sn`",{"HighEnergyPhysics`FeynCalc`"}];

Sn::"usage"= "Sn is Pi^(n/2)/(2 Pi)^n."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

   Sn /:
   MakeBoxes[Sn  ,TraditionalForm] :=
   SubscriptBox["S","n"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Sn | \n "]];
Null
